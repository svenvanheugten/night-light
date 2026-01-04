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

type Light =
    { FriendlyName: string
      Room: Room
      Bulb: Bulb }

let lights =
    [ { FriendlyName = "Vardagsrum - Fönsterlampa"
        Room = LivingRoom
        Bulb = IkeaBulb }
      { FriendlyName = "Vardagsrum - Vägglampa"
        Room = LivingRoom
        Bulb = PaulmannBulb }
      { FriendlyName = "Vardagsrum - Golvlampa"
        Room = LivingRoom
        Bulb = PaulmannBulb }
      { FriendlyName = "Badrum - Taklampa"
        Room = Bathroom
        Bulb = IkeaBulb }
      { FriendlyName = "Sovrum - Nattduksbordlampa"
        Room = Bedroom
        Bulb = IkeaBulb } ]
