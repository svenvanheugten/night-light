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

let internal generateZigbeeCommandToFixLight partOfDay light =
    let color, brightness =
        getDesiredMood light.Room partOfDay |> getDesiredColorAndBrightness light.Bulb

    generateZigbeeCommand color brightness light

type NightLightStateMachine private (maybeTime: DateTime option) =
    new() = NightLightStateMachine None

    member this.OnEventReceived(event: Event) : Result<NightLightStateMachine * Message seq, OnEventReceivedError> =
        result {
            let maybePartOfDay = maybeTime |> Option.map getPartOfDay

            match event, maybePartOfDay with
            | ReceivedZigbeeEvent payload, Some partOfDay ->
                let! zigbeeEvent = parseZigbeeEvent payload |> Result.mapError ParseZigbeeEventError

                return
                    this,
                    match zigbeeEvent with
                    | DeviceAnnounce friendlyName ->
                        let maybeLight = tryFindLight friendlyName

                        match maybeLight with
                        | Some light -> generateZigbeeCommandToFixLight partOfDay light |> Seq.singleton
                        | None -> Seq.empty
                    | ButtonPress action ->
                        let remoteControlledLights = lights |> Seq.filter _.ControlledWithRemote

                        match action with
                        | PressedOn -> remoteControlledLights |> Seq.map (generateStateCommand On)
                        | PressedOff -> remoteControlledLights |> Seq.map (generateStateCommand Off)
            | TimeChanged newTime, maybePartOfDay ->
                let newState = NightLightStateMachine(Some newTime)
                let newPartOfDay = getPartOfDay newTime

                return
                    newState,
                    if maybePartOfDay <> Some newPartOfDay then
                        lights |> Seq.map (generateZigbeeCommandToFixLight newPartOfDay)
                    else
                        Seq.empty
            | _, None -> return! Error TimeIsUnknown
        }
