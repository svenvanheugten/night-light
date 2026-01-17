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

let internal generateZigbeeCommandsForDifference (maybeBefore: NightLightState option) (after: NightLightState) =
    after.LightToState
    |> Seq.collect (fun (KeyValue(light, newState)) ->
        let oldState = maybeBefore |> Option.map _.LightToState[light]

        if oldState <> Some newState then
            generateZigbeeCommandsToFixLight newState light
        else
            Seq.empty)

type NightLightStateMachine private (maybeState: NightLightState option) =
    new() = NightLightStateMachine None

    member this.OnEventReceived(event: Event) : Result<NightLightStateMachine * Message seq, OnEventReceivedError> =
        result {
            match event, maybeState with
            | ReceivedZigbeeEvent payload, Some currentState ->
                let! zigbeeEvent = parseZigbeeEvent payload |> Result.mapError ParseZigbeeEventError

                return
                    match zigbeeEvent with
                    | DeviceAnnounce friendlyName ->
                        let maybeLight = tryFindLight friendlyName

                        this,
                        match maybeLight with
                        | Some light -> generateZigbeeCommandsToFixLight currentState.LightToState[light] light
                        | None -> Seq.empty
                    | ButtonPress action ->
                        let newNightLightState =
                            match action with
                            | PressedOn -> currentState |> withStateForRemoteControlledLights On
                            | PressedOff -> currentState |> withStateForRemoteControlledLights Off
                            | PressedLeft ->
                                let lightThatShouldBeOn =
                                    remoteControlledLights
                                    |> Seq.find (fun light -> light.ControlledWithRemote = RemoteLeft)

                                currentState
                                |> withStateForRemoteControlledLights Off
                                |> withStateFor lightThatShouldBeOn On

                        NightLightStateMachine(Some newNightLightState),
                        generateZigbeeCommandsForDifference (Some currentState) newNightLightState
            | TimeChanged newTime, maybeCurrentState ->
                let newPartOfDay = getPartOfDay newTime

                let newDayStarted =
                    let maybePreviousPartOfDay =
                        maybeCurrentState |> Option.map _.Time |> Option.map getPartOfDay

                    maybePreviousPartOfDay <> Some Day && newPartOfDay = Day

                let newLightToState =
                    lights
                    |> Seq.map (fun light ->
                        let color, brightness =
                            getDesiredMood light.Room newPartOfDay
                            |> getDesiredColorAndBrightness light.Bulb

                        let previousState =
                            maybeCurrentState
                            |> Option.map _.LightToState[light].State
                            |> Option.defaultValue On

                        let newState = if newDayStarted then On else previousState

                        light,
                        { Color = color
                          Brightness = brightness
                          State = newState })
                    |> Map.ofSeq

                let newNightLightState =
                    { Time = newTime
                      LightToState = newLightToState }

                return
                    NightLightStateMachine(Some newNightLightState),
                    generateZigbeeCommandsForDifference maybeCurrentState newNightLightState
            | _, None -> return! Error TimeIsUnknown
        }
