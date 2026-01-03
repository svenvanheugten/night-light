module internal NightLight.Core.ZigbeeCommands

open System.Text.Json.Nodes
open NightLight.Core.Models
open NightLight.Core.Moods

let generateZigbeeCommand friendlyName targetColor targetBrightness =
    let commandObj = JsonObject()

    match targetColor with
    | ColorByCoordinates(x, y) ->
        let colorObj = JsonObject()
        colorObj["x"] <- x
        colorObj["y"] <- y
        commandObj["color"] <- colorObj
    | ColorByTemperature t -> commandObj["color_temp"] <- t

    commandObj["brightness"] <-
        match targetBrightness with
        | Brightness b -> b

    let topic = $"zigbee2mqtt/{friendlyName}/set"
    let payload = commandObj.ToJsonString()

    { Topic = topic; Payload = payload }
