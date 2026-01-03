module internal NightLight.ZigbeeEvents

open NightLight.Models
open FsToolkit.ErrorHandling
open FSharp.Data

type ZigbeeEvent = DeviceAnnounce of FriendlyName: string

let parseZigbeeEvent str =
    result {
        let! jsonValue = JsonValue.TryParse str |> Result.requireSome InvalidJson

        let! messageType = jsonValue.TryGetProperty "type" |> Result.requireSome MissingTypeField
        let! messageData = jsonValue.TryGetProperty "data" |> Result.requireSome MissingDataField

        return!
            match messageType with
            | JsonValue.String "device_announce" ->
                match messageData.TryGetProperty "friendly_name" with
                | Some(JsonValue.String friendlyName) -> Ok(DeviceAnnounce friendlyName)
                | Some _ -> Error InvalidFriendlyNameField
                | None -> Error MissingFriendlyNameField
            | JsonValue.String _ -> Error UnknownType
            | _ -> Error InvalidTypeField
    }
