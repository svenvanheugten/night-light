module NightLight.Core

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
    | PartOfDayChanged

let onEventReceived (partOfDay: PartOfDay) (event: Event) =
    result {
        match event with
        | ReceivedZigbeeEvent payload ->
            let! zigbeeEvent = parseZigbeeEvent payload

            return
                match zigbeeEvent with
                | DeviceAnnounce friendlyName ->
                    let maybeLight = tryFindLight friendlyName

                    match maybeLight with
                    | Some light -> generateZigbeeCommandToFixLight partOfDay light |> Seq.singleton
                    | None -> Seq.empty
        | PartOfDayChanged -> return lights |> Seq.map (generateZigbeeCommandToFixLight partOfDay)
    }
