open System
open System.Text
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open MQTTnet
open MQTTnet.Protocol
open NightLight.PartsOfDay
open NightLight.ZigbeeEvents
open NightLight.ZigbeeCommands
open NightLight.Core

let private generateMqttMessage zigbeeCommand =
    match zigbeeCommand with
    | ZigbeeCommand(topic, payload) ->
        MqttApplicationMessageBuilder()
            .WithTopic(topic)
            .WithPayload(payload)
            .WithQualityOfServiceLevel(MqttQualityOfServiceLevel.AtLeastOnce)
            .Build()

let private publishZigbeeCommands (mqttClient: IMqttClient) (logger: ILogger) (commands: ZigbeeCommand seq) =
    async {
        commands
        |> Seq.iter (fun command ->
            match command with
            | ZigbeeCommand(topic, payload) ->
                logger.LogInformation("Publishing message {Payload} to topic {Topic}...", payload, topic))

        return!
            commands
            |> Seq.map generateMqttMessage
            |> Seq.map mqttClient.PublishAsync
            |> Seq.map Async.AwaitTask
            |> Async.Sequential
            |> Async.Ignore
    }

let private onMqttMessageReceived (mqttClient: IMqttClient) (logger: ILogger) (message: MqttApplicationMessage) =
    let payload = message.Payload
    let decodedPayload = Encoding.UTF8.GetString(&payload)

    logger.LogInformation("Received message with payload {Payload}", decodedPayload)

    let commandsResult =
        decodedPayload |> onZigbeeEventReceived (getPartOfDay DateTime.Now)

    match commandsResult with
    | Ok commands -> publishZigbeeCommands mqttClient logger commands
    | Error UnknownType -> async.Return()
    | Error e ->
        logger.LogError("Error {Error} while processing {Payload}", e, payload)
        async.Return()

[<EntryPoint>]
let mainAsync _ =
    // This is still a stateful mess. Needs to be cleaned up a lot.
    async {
        let loggerFactory =
            LoggerFactory.Create(fun builder -> builder.AddConsole().SetMinimumLevel LogLevel.Information |> ignore)

        let logger = loggerFactory.CreateLogger "NightLight"

        logger.LogInformation("Current system time is {Now}", DateTime.Now)

        let mqttFactory = MqttClientFactory()

        use mqttClient = mqttFactory.CreateMqttClient()

        let server =
            match Environment.GetEnvironmentVariable "MQTT_SERVER" with
            | null -> "localhost"
            | value -> value

        let mqttClientOptions = MqttClientOptionsBuilder().WithTcpServer(server).Build()

        mqttClient.add_ApplicationMessageReceivedAsync (fun e ->
            onMqttMessageReceived mqttClient logger e.ApplicationMessage
            |> Async.StartAsTask
            :> Task)

        do! mqttClient.ConnectAsync mqttClientOptions |> Async.AwaitTask |> Async.Ignore

        do!
            mqttClient.SubscribeAsync "zigbee2mqtt/bridge/event"
            |> Async.AwaitTask
            |> Async.Ignore

        let mutable previousPartOfDay: PartOfDay option = None

        while true do
            let currentPartOfDay = getPartOfDay DateTime.Now

            if previousPartOfDay <> Some currentPartOfDay then
                do! onPartOfDayChanged currentPartOfDay |> publishZigbeeCommands mqttClient logger
                previousPartOfDay <- Some currentPartOfDay

            do! Async.Sleep 10_000

        do! mqttClient.DisconnectAsync() |> Async.AwaitTask
    }
    |> Async.RunSynchronously

    0
