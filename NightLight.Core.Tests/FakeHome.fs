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

type Color =
    | White
    | Yellow
    | Red

type LightState =
    | Off
    | On of Brightness: byte * Color: Color

type FakeLight(light: Light) =
    let mutable hasPower = false
    let mutable brightness: byte = 255uy
    let mutable color: Color = White

    member _.LightWithState = light, if hasPower then On(brightness, color) else Off

    member _.TurnOn() = hasPower <- true

    member _.TurnOff() = hasPower <- false

    member _.SetBrightness(newBrightness: byte) =
        if hasPower then
            brightness <- newBrightness

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
        | HumanInteraction(LightTurnedOn light) ->
            friendlyNameToFakeLight[light.FriendlyName].TurnOn()

            { Topic = "zigbee2mqtt/bridge/event"
              Payload =
                $@"{{
                    ""type"": ""device_announce"",
                    ""data"": {{ ""friendly_name"": ""{light.FriendlyName.Get}"" }}
                  }}" }
            |> ReceivedZigbeeEvent
            |> onEventPublished.Trigger
        | HumanInteraction(LightTurnedOff light) -> friendlyNameToFakeLight[light.FriendlyName].TurnOff()
        | TimeChanged newTime -> newTime |> Event.TimeChanged |> onEventPublished.Trigger

type FakeHome with
    member this.Interact(interactions: Interaction seq) = interactions |> Seq.iter this.Interact

    member this.ForAllLightsThatAreOn condition =
        this.LightStates
        |> Seq.choose (fun (light, state) ->
            match state with
            | On(brightness, color) -> Some(light, brightness, color)
            | Off -> None)
        |> Seq.forall condition
