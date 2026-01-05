module NightLight.Core.Models

open System

type Message = { Topic: string; Payload: string }

type Event =
    | ReceivedZigbeeEvent of Message
    | TimeChanged of DateTime

type ParseZigbeeEventError =
    | UnknownTopic of string
    | InvalidJson
    | MissingTypeField
    | MissingDataField
    | MissingFriendlyNameField
    | InvalidTypeField
    | InvalidFriendlyNameField
    | UnknownType
    | MissingActionField
    | InvalidActionField

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
      Bulb: Bulb
      ControlledWithRemote: bool }

let lights =
    [ { FriendlyName = DeviceFriendlyName "Vardagsrum - Fönsterlampa"
        Room = LivingRoom
        Bulb = IkeaBulb
        ControlledWithRemote = true }
      { FriendlyName = DeviceFriendlyName "Vardagsrum - Vägglampa"
        Room = LivingRoom
        Bulb = PaulmannBulb
        ControlledWithRemote = false }
      { FriendlyName = DeviceFriendlyName "Vardagsrum - Golvlampa"
        Room = LivingRoom
        Bulb = PaulmannBulb
        ControlledWithRemote = false }
      { FriendlyName = DeviceFriendlyName "Badrum - Taklampa"
        Room = Bathroom
        Bulb = IkeaBulb
        ControlledWithRemote = false }
      { FriendlyName = DeviceFriendlyName "Sovrum - Nattduksbordlampa"
        Room = Bedroom
        Bulb = IkeaBulb
        ControlledWithRemote = true } ]

let remoteControlFriendlyName = DeviceFriendlyName "Fjärrkontroll"
