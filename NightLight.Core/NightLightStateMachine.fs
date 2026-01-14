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

let internal generateZigbeeCommandsToFixLight state partOfDay (light: Light) =
    seq {
        match light.ControlledWithRemote, state with
        | NonRemote, On -> ()
        | NonRemote, Off -> failwith $"Unexpectly trying to turn off {light}. It's not remote-controlled."
        | _, _ -> yield generateStateCommand state light

        if state = On then
            let color, brightness =
                getDesiredMood light.Room partOfDay |> getDesiredColorAndBrightness light.Bulb

            yield generateColorCommand light color
            yield generateBrightnessCommand light brightness
    }

type NightLightStateMachine private (maybeTime: DateTime option, lightToState: Map<Light, State>) =
    new() = NightLightStateMachine(None, lights |> Seq.map (fun light -> light, On) |> Map.ofSeq)

    member this.OnEventReceived(event: Event) : Result<NightLightStateMachine * Message seq, OnEventReceivedError> =
        result {
            let maybePartOfDay = maybeTime |> Option.map getPartOfDay

            let remoteControlledLights =
                lights |> Seq.filter (not << _.ControlledWithRemote.IsNonRemote)

            let updateLightStateForRemoteControlledLights desiredLightState =
                remoteControlledLights
                |> Seq.fold (fun acc key -> Map.add key desiredLightState acc) lightToState

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
                        let newLightToState =
                            match action with
                            | PressedOn -> updateLightStateForRemoteControlledLights On
                            | PressedOff -> updateLightStateForRemoteControlledLights Off
                            | PressedLeft ->
                                updateLightStateForRemoteControlledLights Off
                                |> Map.add
                                    (lights |> Seq.find (fun light -> light.ControlledWithRemote = RemoteLeft))
                                    On

                        NightLightStateMachine(maybeTime, newLightToState),
                        remoteControlledLights
                        |> Seq.collect (fun light ->
                            generateZigbeeCommandsToFixLight newLightToState[light] partOfDay light)
            | TimeChanged newTime, maybePartOfDay ->
                let newPartOfDay = getPartOfDay newTime

                let partOfDayChanged = maybePartOfDay <> Some newPartOfDay

                let newLightToState =
                    if partOfDayChanged && newPartOfDay = Day then
                        updateLightStateForRemoteControlledLights On
                    else
                        lightToState

                let newState = NightLightStateMachine(Some newTime, newLightToState)

                return
                    newState,
                    if partOfDayChanged then
                        lights
                        |> Seq.collect (fun light ->
                            generateZigbeeCommandsToFixLight newLightToState[light] newPartOfDay light)
                    else
                        Seq.empty
            | _, None -> return! Error TimeIsUnknown
        }
