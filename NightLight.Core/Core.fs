module NightLight.Core

open System
open NightLight.PartsOfDay
open NightLight.ZigbeeEvents
open NightLight.ZigbeeCommands
open NightLight.Moods
open NightLight.Lights
open NightLight.Configuration
open FsToolkit.ErrorHandling

let internal tryFindLight friendlyName =
    Seq.tryFind (fun light -> light.FriendlyName = friendlyName) lights

let internal generateZigbeeCommandToFixLight partOfDay light =
    let color, brightness =
        getDesiredMood light.Room partOfDay |> getDesiredColorAndBrightness light.Bulb

    generateZigbeeCommand light.FriendlyName color brightness

type Event =
    | ReceivedZigbeeEvent of payload: string
    | TimeChanged of DateTime

type ParseEventError = ParseZigbeeEventError of ParseZigbeeEventError

type State = { Time: DateTime }

let onEventReceived (state: State) (event: Event) : Result<State * ZigbeeCommand seq, ParseEventError> =
    result {
        let partOfDay = getPartOfDay state.Time

        match event with
        | ReceivedZigbeeEvent payload ->
            let! zigbeeEvent = parseZigbeeEvent payload |> Result.mapError ParseZigbeeEventError

            return
                state,
                match zigbeeEvent with
                | DeviceAnnounce friendlyName ->
                    let maybeLight = tryFindLight friendlyName

                    match maybeLight with
                    | Some light -> generateZigbeeCommandToFixLight partOfDay light |> Seq.singleton
                    | None -> Seq.empty
        | TimeChanged time ->
            let newState = { Time = time }
            let newPartOfDay = getPartOfDay time

            return
                newState,
                if partOfDay <> newPartOfDay then
                    lights |> Seq.map (generateZigbeeCommandToFixLight newPartOfDay)
                else
                    Seq.empty
    }
