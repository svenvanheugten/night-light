namespace NightLight.Core.Tests

open System
open System.Text.RegularExpressions
open NightLight.Core.Models
open FsToolkit.ErrorHandling
open FSharp.Data

type HumanInteraction =
    | LightTurnedOn of Light
    | LightTurnedOff of Light

type Interaction =
    | HumanInteraction of HumanInteraction
    | TimeChanged of DateTime

type LightState =
    | Off
    | On of Brightness: byte

type FakeLight(light: Light) =
    let mutable hasPower = false
    let mutable brightness: byte = 255uy

    member _.LightWithState = light, if hasPower then On brightness else Off

    member _.TurnOn() = hasPower <- true

    member _.TurnOff() = hasPower <- false

    member _.SetBrightness(newBrightness: byte) =
        if hasPower then
            brightness <- newBrightness

type FakeHome(now: DateTime) =
    let nightLightStateMachine = NightLightStateMachine now

    let friendlyNameToFakeLight =
        lights
        |> Seq.map (fun light -> light.FriendlyName, FakeLight light)
        |> Map.ofSeq

    let processCommand command =
        option {
            let! friendlyName =
                let m = Regex.Match(command.Topic, "^zigbee2mqtt/(.+)/set$")
                if m.Success then Some m.Groups.[1].Value else None

            let! fakeLight = Map.tryFind friendlyName friendlyNameToFakeLight

            let parsedPayload = JsonValue.Parse command.Payload

            match parsedPayload.TryGetProperty "brightness" with
            | Some(JsonValue.Number newBrightness) -> fakeLight.SetBrightness(byte newBrightness)
            | None -> ()
            | value -> failwith $"Unexpected brightness value {value}"
        }
        |> ignore

    member _.LightStates = friendlyNameToFakeLight.Values |> Seq.map _.LightWithState

    member _.Interact(interaction: Interaction) =
        match interaction with
        | HumanInteraction(LightTurnedOn light) ->
            friendlyNameToFakeLight[light.FriendlyName].TurnOn()

            { Topic = "zigbee2mqtt/bridge/event"
              Payload =
                $@"{{
                    ""type"": ""device_announce"",
                    ""data"": {{ ""friendly_name"": ""{light.FriendlyName}"" }}
                  }}" }
            |> nightLightStateMachine.SendMessage
        | HumanInteraction(LightTurnedOff light) -> friendlyNameToFakeLight[light.FriendlyName].TurnOff()
        | TimeChanged time -> nightLightStateMachine.ChangeTime time

        nightLightStateMachine.TransmittedCommands |> Seq.iter processCommand
        nightLightStateMachine.ClearTransmittedCommands()
