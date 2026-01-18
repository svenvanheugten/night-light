namespace NightLight.Core.Tests

open System
open System.Text.RegularExpressions
open NightLight.Core.Models
open FsToolkit.ErrorHandling
open FSharp.Data

type RemoteInteraction =
    | RemotePressedOnButton
    | RemotePressedOffButton
    | RemotePressedLeftButton

type HumanInteraction =
    | LightPoweredOn of Light
    | LightPoweredOff of Light

type Interaction =
    | HumanInteraction of HumanInteraction
    | RemoteInteraction of RemoteInteraction
    | TimeChanged of DateTime

type Color =
    | White
    | Yellow
    | Red

type LightState =
    | Off
    | On of Brightness: byte * Color: Color

type FakeLight(light: Light) =
    let mutable hasPower = false
    let mutable state = true
    let mutable brightness: byte = 255uy
    let mutable color: Color = White

    member _.LightWithState =
        light, if hasPower && state then On(brightness, color) else Off

    member _.PowerOn() = hasPower <- true

    member _.PowerOff() = hasPower <- false

    member _.SetState(newState: bool) =
        if hasPower then
            state <- newState

    member _.SetBrightness(newBrightness: byte) =
        if hasPower then
            brightness <- newBrightness

            if light.Bulb = IkeaBulb then
                state <- true

    member _.SetColor(newColor: Color) =
        if hasPower then
            color <- newColor

type FakeHome() =
    let friendlyNameToFakeLight =
        lights
        |> Seq.map (fun light -> light.FriendlyName, FakeLight light)
        |> Map.ofSeq

    let onEventPublished = new Event<Event>()

    member _.LightStates = friendlyNameToFakeLight.Values |> Seq.map _.LightWithState

    [<CLIEvent>]
    member _.OnEventPublished = onEventPublished.Publish

    member _.ProcessCommand(command: Message) =
        option {
            let! friendlyName =
                let m = Regex.Match(command.Topic, "^zigbee2mqtt/(.+)/set$")

                if m.Success then
                    Some(DeviceFriendlyName m.Groups.[1].Value)
                else
                    None

            let! fakeLight = Map.tryFind friendlyName friendlyNameToFakeLight

            let parsedPayload = JsonValue.Parse command.Payload

            match parsedPayload.TryGetProperty "state" with
            | Some(JsonValue.String "ON") -> fakeLight.SetState true
            | Some(JsonValue.String "OFF") -> fakeLight.SetState false
            | None -> ()
            | value -> failwith $"Unexpected state value {value}"

            match parsedPayload.TryGetProperty "brightness" with
            | Some(JsonValue.Number newBrightness) -> fakeLight.SetBrightness(byte newBrightness)
            | None -> ()
            | value -> failwith $"Unexpected brightness value {value}"

            match parsedPayload.TryGetProperty "color" with
            | Some color ->
                match color.TryGetProperty "x", color.TryGetProperty "y" with
                | Some(JsonValue.Number 0.3227M), Some(JsonValue.Number 0.329M) -> fakeLight.SetColor White
                | Some(JsonValue.Number 0.6942M), Some(JsonValue.Number 0.2963M) -> fakeLight.SetColor Red
                | _ -> failwith $"Unexpected color value {color}"
            | None -> ()

            match parsedPayload.TryGetProperty "color_temp" with
            | Some(JsonValue.Number temperature) when temperature = 454M -> fakeLight.SetColor Yellow
            | None -> ()
            | value -> failwith $"Unexpected color temperature value {value}"
        }
        |> ignore

    member _.Interact(interaction: Interaction) =
        match interaction with
        | HumanInteraction(LightPoweredOn light) ->
            friendlyNameToFakeLight[light.FriendlyName].PowerOn()

            { Topic = "zigbee2mqtt/bridge/event"
              Payload =
                $@"{{
                    ""type"": ""device_announce"",
                    ""data"": {{ ""friendly_name"": ""{light.FriendlyName.Get}"" }}
                  }}" }
            |> ReceivedZigbeeEvent
            |> onEventPublished.Trigger
        | HumanInteraction(LightPoweredOff light) -> friendlyNameToFakeLight[light.FriendlyName].PowerOff()
        | RemoteInteraction RemotePressedOnButton ->
            { Topic = $"zigbee2mqtt/{remoteControlFriendlyName.Get}"
              Payload = @"{ ""action"": ""on"" }" }
            |> ReceivedZigbeeEvent
            |> onEventPublished.Trigger
        | RemoteInteraction RemotePressedOffButton ->
            { Topic = $"zigbee2mqtt/{remoteControlFriendlyName.Get}"
              Payload = @"{ ""action"": ""off"" }" }
            |> ReceivedZigbeeEvent
            |> onEventPublished.Trigger
        | RemoteInteraction RemotePressedLeftButton ->
            { Topic = $"zigbee2mqtt/{remoteControlFriendlyName.Get}"
              Payload = @"{ ""action"": ""arrow_left_click"" }" }
            |> ReceivedZigbeeEvent
            |> onEventPublished.Trigger
        | TimeChanged newTime -> newTime |> Event.TimeChanged |> onEventPublished.Trigger

type FakeHome with
    member this.Interact(interactions: Interaction seq) = interactions |> Seq.iter this.Interact

    member this.LightsThatAreOn =
        this.LightStates |> Seq.filter (snd >> _.IsOn) |> Seq.toList

    member this.NonRemotelyControlledLightStates =
        this.LightStates
        |> Seq.filter (fst >> _.ControlledWithRemote >> (=) NonRemote)
        |> Seq.toList

    member this.RemotelyControlledLightStates =
        this.LightStates
        |> Seq.filter (fst >> _.ControlledWithRemote >> (<>) NonRemote)
        |> Seq.toList

    member this.Label =
        this.LightsThatAreOn
        |> Seq.map (fun (light, state) -> $"{light.FriendlyName.Get}: {state}")
        |> String.concat ", "
