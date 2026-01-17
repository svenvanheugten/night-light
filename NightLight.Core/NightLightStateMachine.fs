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
      LightToState: Map<Light, LightState> }

type NightLightStateMachine private (maybeState: NightLightState option) =
    new() = NightLightStateMachine None

    member this.OnEventReceived(event: Event) : Result<NightLightStateMachine * Message seq, OnEventReceivedError> =
        result {
            let updateLightStateForRemoteControlledLights (oldLightToState: Map<Light, LightState>) desiredLightState =
                remoteControlledLights
                |> Seq.fold
                    (fun acc key ->
                        let oldState = oldLightToState[key]

                        Map.add
                            key
                            { oldState with
                                State = desiredLightState }
                            acc)
                    oldLightToState

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
                        | Some light -> generateZigbeeCommandsToFixLight lightToState[light].State partOfDay light
                        | None -> Seq.empty
                    | ButtonPress action ->
                        let newLightToState =
                            match action with
                            | PressedOn -> updateLightStateForRemoteControlledLights lightToState On
                            | PressedOff -> updateLightStateForRemoteControlledLights lightToState Off
                            | PressedLeft ->
                                let lightThatShouldBeOn =
                                    remoteControlledLights
                                    |> Seq.find (fun light -> light.ControlledWithRemote = RemoteLeft)

                                let oldState = lightToState[lightThatShouldBeOn]

                                updateLightStateForRemoteControlledLights lightToState Off
                                |> Map.add lightThatShouldBeOn { oldState with State = On }

                        NightLightStateMachine(
                            Some
                            <| { Time = time
                                 LightToState = newLightToState }
                        ),
                        remoteControlledLights
                        |> Seq.collect (fun light ->
                            generateZigbeeCommandsToFixLight newLightToState[light].State partOfDay light)
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
                            generateZigbeeCommandsToFixLight newLightToState[light].State newPartOfDay light)
                    else
                        Seq.empty
            | _, None -> return! Error TimeIsUnknown
        }
