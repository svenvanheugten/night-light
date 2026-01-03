module internal NightLight.ZigbeeCommands

open System.Text.Json.Nodes
open NightLight.Models
open NightLight.Lights

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

    Message(topic, payload)
