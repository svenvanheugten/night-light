module NightLight.Core.Core

open System
open NightLight.Core.Models
open NightLight.Core.PartsOfDay
open NightLight.Core.ZigbeeEvents
open NightLight.Core.ZigbeeCommands
open NightLight.Core.Moods
open FsToolkit.ErrorHandling

let internal tryFindLight friendlyName =
    Seq.tryFind (fun light -> light.FriendlyName = friendlyName) lights

let internal generateZigbeeCommandsToFixLight state partOfDay light =
    let color, brightness =
        getDesiredMood light.Room partOfDay |> getDesiredColorAndBrightness light.Bulb

    seq {
        generateStateCommand state light
        generateZigbeeCommand color brightness light
    }

type NightLightStateMachine private (maybeTime: DateTime option, lightToState: Map<Light, State>) =
    new() = NightLightStateMachine(None, lights |> Seq.map (fun light -> light, On) |> Map.ofSeq)

    member this.OnEventReceived(event: Event) : Result<NightLightStateMachine * Message seq, OnEventReceivedError> =
        result {
            let maybePartOfDay = maybeTime |> Option.map getPartOfDay

            match event, maybePartOfDay with
            | ReceivedZigbeeEvent payload, Some partOfDay ->
                let! zigbeeEvent = parseZigbeeEvent payload |> Result.mapError ParseZigbeeEventError

                return
                    match zigbeeEvent with
                    | DeviceAnnounce friendlyName ->
                        let maybeLight = tryFindLight friendlyName

                        this,
                        match maybeLight with
                        | Some light -> generateZigbeeCommandsToFixLight lightToState[light] partOfDay light
                        | None -> Seq.empty
                    | ButtonPress action ->
                        let desiredLightState =
                            match action with
                            | PressedOn -> On
                            | PressedOff -> Off

                        let remoteControlledLights = lights |> Seq.filter _.ControlledWithRemote

                        let newLightToState =
                            remoteControlledLights
                            |> Seq.fold (fun acc key -> Map.add key desiredLightState acc) lightToState

                        NightLightStateMachine(maybeTime, newLightToState),
                        remoteControlledLights |> Seq.map (generateStateCommand desiredLightState)
            | TimeChanged newTime, maybePartOfDay ->
                let newState = NightLightStateMachine(Some newTime, lightToState)
                let newPartOfDay = getPartOfDay newTime

                return
                    newState,
                    if maybePartOfDay <> Some newPartOfDay then
                        lights
                        |> Seq.collect (fun light ->
                            generateZigbeeCommandsToFixLight lightToState[light] newPartOfDay light)
                    else
                        Seq.empty
            | _, None -> return! Error TimeIsUnknown
        }
