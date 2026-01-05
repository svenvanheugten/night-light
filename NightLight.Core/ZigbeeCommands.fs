module internal NightLight.Core.ZigbeeCommands

open System.Text.Json.Nodes
open NightLight.Core.Models
open NightLight.Core.Moods

type State =
    | On
    | Off

let toZigbeeCommand light payload =
    let topic = $"zigbee2mqtt/{light.FriendlyName.Get}/set"
    { Topic = topic; Payload = payload }

let generateStateCommand state light =
    let commandObj = JsonObject()

    commandObj["state"] <-
        match state with
        | On -> "ON"
        | Off -> "OFF"

    if light.Bulb = IkeaBulb then
        commandObj["transition"] <- 0

    commandObj.ToJsonString() |> toZigbeeCommand light

let generateZigbeeCommand targetColor targetBrightness light =
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

    commandObj.ToJsonString() |> toZigbeeCommand light
