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

let internal generateZigbeeCommandsToFixLight state (light: Light) =
    seq {
        match light.ControlledWithRemote, state.State with
        | NonRemote, On -> ()
        | NonRemote, Off -> failwith $"Unexpectly trying to turn off {light}. It's not remote-controlled."
        | _, _ -> yield generateStateCommand state.State light

        if state.State = On then
            yield generateColorCommand light state.Color
            yield generateBrightnessCommand light state.Brightness
    }

type internal NightLightState =
    { Time: DateTime
      LightToState: Map<Light, LightState> }

let internal withStateFor (light: Light) (state: State) (oldNightLightState: NightLightState) =
    let oldState = oldNightLightState.LightToState[light]

    { oldNightLightState with
        LightToState = Map.add light { oldState with State = state } oldNightLightState.LightToState }

let internal withStateForRemoteControlledLights (state: State) (oldNightLightState: NightLightState) =
    remoteControlledLights
    |> Seq.fold (fun acc light -> acc |> withStateFor light state) oldNightLightState

type NightLightStateMachine private (maybeState: NightLightState option) =
    new() = NightLightStateMachine None

    member this.OnEventReceived(event: Event) : Result<NightLightStateMachine * Message seq, OnEventReceivedError> =
        result {
            match event, maybeState with
            | ReceivedZigbeeEvent payload, Some state ->
                let! zigbeeEvent = parseZigbeeEvent payload |> Result.mapError ParseZigbeeEventError

                return
                    match zigbeeEvent with
                    | DeviceAnnounce friendlyName ->
                        let maybeLight = tryFindLight friendlyName

                        this,
                        match maybeLight with
                        | Some light -> generateZigbeeCommandsToFixLight state.LightToState[light] light
                        | None -> Seq.empty
                    | ButtonPress action ->
                        let newState =
                            match action with
                            | PressedOn -> state |> withStateForRemoteControlledLights On
                            | PressedOff -> state |> withStateForRemoteControlledLights Off
                            | PressedLeft ->
                                let lightThatShouldBeOn =
                                    remoteControlledLights
                                    |> Seq.find (fun light -> light.ControlledWithRemote = RemoteLeft)

                                state
                                |> withStateForRemoteControlledLights Off
                                |> withStateFor lightThatShouldBeOn On

                        NightLightStateMachine(Some newState),
                        remoteControlledLights
                        |> Seq.collect (fun light ->
                            generateZigbeeCommandsToFixLight newState.LightToState[light] light)
            | TimeChanged newTime, maybeState ->
                let newPartOfDay = getPartOfDay newTime

                let partOfDayChanged =
                    let maybePreviousPartOfDay =
                        maybeState |> Option.map _.Time |> Option.map getPartOfDay

                    maybePreviousPartOfDay <> Some newPartOfDay

                let newLightToState =
                    lights
                    |> Seq.map (fun light ->
                        let color, brightness =
                            getDesiredMood light.Room newPartOfDay
                            |> getDesiredColorAndBrightness light.Bulb

                        let previousState =
                            maybeState |> Option.map _.LightToState[light].State |> Option.defaultValue On

                        let newState =
                            if partOfDayChanged && newPartOfDay = Day then
                                On
                            else
                                previousState

                        light,
                        { Color = color
                          Brightness = brightness
                          State = newState })
                    |> Map.ofSeq

                let newNightLightStateMachine =
                    { Time = newTime
                      LightToState = newLightToState }
                    |> Some
                    |> NightLightStateMachine

                return
                    newNightLightStateMachine,
                    if partOfDayChanged then
                        lights
                        |> Seq.collect (fun light -> generateZigbeeCommandsToFixLight newLightToState[light] light)
                    else
                        Seq.empty
            | _, None -> return! Error TimeIsUnknown
        }
