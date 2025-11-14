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

let onZigbeeEventReceived (partOfDay: PartOfDay) (decodedPayload: string) =
    result {
        let! zigbeeEvent = parseZigbeeEvent decodedPayload

        return
            match zigbeeEvent with
            | DeviceAnnounce friendlyName ->
                let maybeLight = tryFindLight friendlyName

                match maybeLight with
                | Some light -> generateZigbeeCommandToFixLight partOfDay light |> Seq.singleton
                | None -> Seq.empty
    }

let onPartOfDayChanged (partOfDay: PartOfDay) =
    lights |> Seq.map (generateZigbeeCommandToFixLight partOfDay)
