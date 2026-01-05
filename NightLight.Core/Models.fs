module NightLight.Core.Models

open System

type Message = { Topic: string; Payload: string }

type Event =
    | ReceivedZigbeeEvent of Message
    | TimeChanged of DateTime

type ParseZigbeeEventError =
    | InvalidJson
    | MissingTypeField
    | MissingDataField
    | MissingFriendlyNameField
    | InvalidTypeField
    | InvalidFriendlyNameField
    | UnknownType

type OnEventReceivedError =
    | ParseZigbeeEventError of ParseZigbeeEventError
    | TimeIsUnknown

type Room =
    | Bathroom
    | LivingRoom
    | Bedroom

type Bulb =
    | IkeaBulb
    | PaulmannBulb

type DeviceFriendlyName =
    | DeviceFriendlyName of string

    member this.Get =
        match this with
        | DeviceFriendlyName deviceFriendlyName -> deviceFriendlyName

type Light =
    { FriendlyName: DeviceFriendlyName
      Room: Room
      Bulb: Bulb }

let lights =
    [ { FriendlyName = DeviceFriendlyName "Vardagsrum - Fönsterlampa"
        Room = LivingRoom
        Bulb = IkeaBulb }
      { FriendlyName = DeviceFriendlyName "Vardagsrum - Vägglampa"
        Room = LivingRoom
        Bulb = PaulmannBulb }
      { FriendlyName = DeviceFriendlyName "Vardagsrum - Golvlampa"
        Room = LivingRoom
        Bulb = PaulmannBulb }
      { FriendlyName = DeviceFriendlyName "Badrum - Taklampa"
        Room = Bathroom
        Bulb = IkeaBulb }
      { FriendlyName = DeviceFriendlyName "Sovrum - Nattduksbordlampa"
        Room = Bedroom
        Bulb = IkeaBulb } ]
