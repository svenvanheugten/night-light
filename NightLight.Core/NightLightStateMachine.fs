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

    generateZigbeeCommand light.FriendlyName color brightness

type NightLightStateMachine(time: DateTime) =
    member this.OnEventReceived(event: Event) : Result<NightLightStateMachine * Message seq, ParseEventError> =
        result {
            let partOfDay = getPartOfDay time

            match event with
            | ReceivedZigbeeEvent payload ->
                let! zigbeeEvent = parseZigbeeEvent payload |> Result.mapError ParseZigbeeEventError

                return
                    this,
                    match zigbeeEvent with
                    | DeviceAnnounce friendlyName ->
                        let maybeLight = tryFindLight friendlyName

                        match maybeLight with
                        | Some light -> generateZigbeeCommandToFixLight partOfDay light |> Seq.singleton
                        | None -> Seq.empty
            | TimeChanged newTime ->
                let newState = NightLightStateMachine newTime
                let newPartOfDay = getPartOfDay newTime

                return
                    newState,
                    if partOfDay <> newPartOfDay then
                        lights |> Seq.map (generateZigbeeCommandToFixLight newPartOfDay)
                    else
                        Seq.empty
        }
