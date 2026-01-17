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

let internal generateZigbeeCommandsToFixLight (light: Light) (desiredLightState: LightState) =
    seq {
        match light.ControlledWithRemote, desiredLightState.State with
        | NonRemote, On -> ()
        | NonRemote, Off -> failwith $"Unexpectly trying to turn off {light}. It's not remote-controlled."
        | _, On when light.Bulb = IkeaBulb -> () // Rely on the brightness command for turning it on
        | _, _ -> yield generateStateCommand desiredLightState.State light

        if desiredLightState.State = On then
            yield generateBrightnessCommand light desiredLightState.Brightness
            yield generateColorCommand light desiredLightState.Color
    }

type internal NightLightState =
    { Time: DateTime
      Alarm: bool
      LightToState: Map<Light, LightState> }

let internal createOrUpdateNightLightState
    (time: DateTime)
    (alarm: bool)
    (maybeOldLightToState: Map<Light, LightState> option)
    =
    let partOfDay = getPartOfDay time

    let lightToState =
        lights
        |> Seq.map (fun light ->
            let color, brightness =
                getDesiredMood light.Room partOfDay |> getDesiredColorAndBrightness light.Bulb

            let previousState =
                maybeOldLightToState
                |> Option.map (fun lightToState -> lightToState[light].State)
                |> Option.defaultValue On

            light,
            { Color = color
              Brightness =
                if alarm && light.ControlledWithRemote <> NonRemote then
                    brightness.Scale(getAlarmWeight time)
                else
                    brightness
              State = if alarm then On else previousState })
        |> Map.ofSeq

    { Time = time
      Alarm = alarm
      LightToState = lightToState }

let internal withStateFor (light: Light) (state: State) (oldNightLightState: NightLightState) =
    let oldState = oldNightLightState.LightToState[light]

    createOrUpdateNightLightState
        oldNightLightState.Time
        oldNightLightState.Alarm
        (Map.add light { oldState with State = state } oldNightLightState.LightToState
         |> Some)

let internal withStateForRemoteControlledLights (state: State) (oldNightLightState: NightLightState) =
    lights
    |> Seq.filter (not << _.ControlledWithRemote.IsNonRemote)
    |> Seq.fold (fun acc light -> acc |> withStateFor light state) oldNightLightState

let internal withAlarmOff (oldNightLightState: NightLightState) =
    createOrUpdateNightLightState oldNightLightState.Time false (Some oldNightLightState.LightToState)

let internal generateZigbeeCommandsForDifference (maybeBefore: NightLightState option) (after: NightLightState) =
    after.LightToState
    |> Seq.collect (fun (KeyValue(light, newState)) ->
        let oldState = maybeBefore |> Option.map _.LightToState[light]

        if oldState <> Some newState then
            generateZigbeeCommandsToFixLight light after.LightToState[light]
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
                        | Some light -> generateZigbeeCommandsToFixLight light currentState.LightToState[light]
                        | None -> Seq.empty
                    | ButtonPress action ->
                        let newNightLightState =
                            match action with
                            | PressedOn -> currentState |> withAlarmOff |> withStateForRemoteControlledLights On
                            | PressedOff -> currentState |> withAlarmOff |> withStateForRemoteControlledLights Off
                            | PressedLeft ->
                                let lightThatShouldBeOn =
                                    lights |> Seq.find (fun light -> light.ControlledWithRemote = RemoteLeft)

                                currentState
                                |> withAlarmOff
                                |> withStateForRemoteControlledLights Off
                                |> withStateFor lightThatShouldBeOn On

                        NightLightStateMachine(Some newNightLightState),
                        generateZigbeeCommandsForDifference (Some currentState) newNightLightState
            | TimeChanged newTime, maybeCurrentState ->
                let alarm =
                    let newDayStarted =
                        let newPartOfDay = getPartOfDay newTime

                        let maybePreviousPartOfDay =
                            maybeCurrentState |> Option.map _.Time |> Option.map getPartOfDay

                        maybePreviousPartOfDay <> Some Day && newPartOfDay = Day

                    newDayStarted
                    || maybeCurrentState |> Option.map _.Alarm |> Option.defaultValue false

                let newNightLightState =
                    createOrUpdateNightLightState newTime alarm (maybeCurrentState |> Option.map _.LightToState)

                return
                    NightLightStateMachine(Some newNightLightState),
                    generateZigbeeCommandsForDifference maybeCurrentState newNightLightState
            | _, None -> return! Error TimeIsUnknown
        }
