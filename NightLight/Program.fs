open System
open System.Text
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open MQTTnet
open MQTTnet.Protocol
open NightLight.Core.Models
open NightLight.Core.Core

let private generateMqttMessage zigbeeCommand =
    MqttApplicationMessageBuilder()
        .WithTopic(zigbeeCommand.Topic)
        .WithPayload(zigbeeCommand.Payload)
        .WithQualityOfServiceLevel(MqttQualityOfServiceLevel.AtLeastOnce)
        .Build()

let private publishZigbeeCommands (mqttClient: IMqttClient) (logger: ILogger) (commands: Message seq) =
    async {
        commands
        |> Seq.iter (fun command ->
            logger.LogInformation("Publishing message {Payload} to topic {Topic}...", command.Payload, command.Topic))

        return!
            commands
            |> Seq.map generateMqttMessage
            |> Seq.map (fun message -> async { return! mqttClient.PublishAsync message |> Async.AwaitTask })
            |> Async.Sequential
            |> Async.Ignore
    }

let private handleEvent (mqttClient: IMqttClient) (logger: ILogger) (state: NightLightStateMachine) (event: Event) =
    match event with
    | ReceivedZigbeeEvent payload -> logger.LogInformation("Received message with payload {Payload}", payload)
    | _ -> ()

    let result = event |> state.OnEventReceived

    match result with
    | Ok(newState, commands) ->
        async {
            do! publishZigbeeCommands mqttClient logger commands
            return newState
        }
    | Error(ParseZigbeeEventError UnknownType) -> async.Return state
    | Error e ->
        logger.LogError("Error {Error} while {Event}", e, event)
        async.Return state

let private decodeMqttApplicationMessage (message: MqttApplicationMessage) =
    let payload = message.Payload
    let decodedPayload = Encoding.UTF8.GetString(&payload)

    { Topic = message.Topic
      Payload = decodedPayload }

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

        let stateLock = new SemaphoreSlim(1, 1)

        let! initialState =
            let emptyNightLightStateMachine = NightLightStateMachine()

            TimeChanged DateTime.Now
            |> handleEvent mqttClient logger emptyNightLightStateMachine

        let mutable state = initialState

        mqttClient.add_ApplicationMessageReceivedAsync (fun e ->
            async {
                let event = ReceivedZigbeeEvent <| decodeMqttApplicationMessage e.ApplicationMessage

                do! stateLock.WaitAsync() |> Async.AwaitTask

                try
                    let! newState = event |> handleEvent mqttClient logger state
                    state <- newState
                finally
                    stateLock.Release() |> ignore
            }
            |> Async.StartAsTask
            :> Task)

        do! mqttClient.ConnectAsync mqttClientOptions |> Async.AwaitTask |> Async.Ignore

        do!
            mqttClient.SubscribeAsync "zigbee2mqtt/bridge/event"
            |> Async.AwaitTask
            |> Async.Ignore

        while true do
            do! stateLock.WaitAsync() |> Async.AwaitTask

            try
                let! newState = TimeChanged DateTime.Now |> handleEvent mqttClient logger state
                state <- newState
            finally
                stateLock.Release() |> ignore

            do! Async.Sleep 10_000

        do! mqttClient.DisconnectAsync() |> Async.AwaitTask
    }
    |> Async.RunSynchronously

    0
