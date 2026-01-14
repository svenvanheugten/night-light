module internal NightLight.Core.ZigbeeEvents

open NightLight.Core.Models
open FsToolkit.ErrorHandling
open FSharp.Data

type Action =
    | PressedOn
    | PressedOff
    | PressedLeft

type ZigbeeEvent =
    | DeviceAnnounce of DeviceFriendlyName
    | ButtonPress of Action

let parseZigbeeEvent (message: Message) =
    result {
        let! jsonValue = JsonValue.TryParse message.Payload |> Result.requireSome InvalidJson

        match message.Topic with
        | "zigbee2mqtt/bridge/event" ->
            let! messageType = jsonValue.TryGetProperty "type" |> Result.requireSome MissingTypeField
            let! messageData = jsonValue.TryGetProperty "data" |> Result.requireSome MissingDataField

            return!
                match messageType with
                | JsonValue.String "device_announce" ->
                    match messageData.TryGetProperty "friendly_name" with
                    | Some(JsonValue.String friendlyName) -> Ok <| DeviceAnnounce(DeviceFriendlyName friendlyName)
                    | Some _ -> Error InvalidFriendlyNameField
                    | None -> Error MissingFriendlyNameField
                | JsonValue.String _ -> Error UnknownType
                | _ -> Error InvalidTypeField
        | "zigbee2mqtt/Fjärrkontroll" ->
            return!
                match jsonValue.TryGetProperty "action" with
                | Some(JsonValue.String "on") -> Ok(ButtonPress PressedOn)
                | Some(JsonValue.String "off") -> Ok(ButtonPress PressedOff)
                | Some(JsonValue.String "arrow_left_click") -> Ok(ButtonPress PressedLeft)
                | Some _ -> Error InvalidActionField
                | None -> Error MissingActionField
        | _ -> return! Error <| UnknownTopic message.Topic
    }
