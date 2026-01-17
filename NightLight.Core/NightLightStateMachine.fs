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

type internal NightLightState =
    { Time: DateTime
      LightToState: Map<Light, State> }

type NightLightStateMachine private (maybeState: NightLightState option) =
    new() = NightLightStateMachine None

    member this.OnEventReceived(event: Event) : Result<NightLightStateMachine * Message seq, OnEventReceivedError> =
        result {
            let updateLightStateForRemoteControlledLights oldLightToState desiredLightState =
                remoteControlledLights
                |> Seq.fold (fun acc key -> Map.add key desiredLightState acc) oldLightToState

            match event, maybeState with
            | ReceivedZigbeeEvent payload,
              Some { Time = time
                     LightToState = lightToState } ->
                let partOfDay = time |> getPartOfDay
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
                            | PressedOn -> updateLightStateForRemoteControlledLights lightToState On
                            | PressedOff -> updateLightStateForRemoteControlledLights lightToState Off
                            | PressedLeft ->
                                updateLightStateForRemoteControlledLights lightToState Off
                                |> Map.add
                                    (remoteControlledLights
                                     |> Seq.find (fun light -> light.ControlledWithRemote = RemoteLeft))
                                    On

                        NightLightStateMachine(
                            Some
                            <| { Time = time
                                 LightToState = newLightToState }
                        ),
                        remoteControlledLights
                        |> Seq.collect (fun light ->
                            generateZigbeeCommandsToFixLight newLightToState[light] partOfDay light)
            | TimeChanged newTime, maybeState ->
                let newPartOfDay = getPartOfDay newTime

                let partOfDayChanged =
                    let maybePreviousPartOfDay =
                        maybeState |> Option.map _.Time |> Option.map getPartOfDay

                    maybePreviousPartOfDay <> Some newPartOfDay

                let newLightToState =
                    lights
                    |> Seq.map (fun light ->
                        let previousState =
                            maybeState |> Option.map _.LightToState[light] |> Option.defaultValue On

                        let newState =
                            if partOfDayChanged && newPartOfDay = Day then
                                On
                            else
                                previousState

                        light, newState)
                    |> Map.ofSeq

                let newState =
                    NightLightStateMachine(
                        Some
                        <| { Time = newTime
                             LightToState = newLightToState }
                    )

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
