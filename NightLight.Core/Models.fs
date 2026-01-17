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

type LightControl =
    | NonRemote
    | RemoteLeft
    | RemoteRight

type Light =
    { FriendlyName: DeviceFriendlyName
      Room: Room
      Bulb: Bulb
      ControlledWithRemote: LightControl }

let lights =
    [ { FriendlyName = DeviceFriendlyName "Vardagsrum - Fönsterlampa"
        Room = Bedroom
        Bulb = IkeaBulb
        ControlledWithRemote = RemoteRight }
      { FriendlyName = DeviceFriendlyName "Vardagsrum - Vägglampa"
        Room = LivingRoom
        Bulb = PaulmannBulb
        ControlledWithRemote = NonRemote }
      { FriendlyName = DeviceFriendlyName "Vardagsrum - Golvlampa"
        Room = LivingRoom
        Bulb = PaulmannBulb
        ControlledWithRemote = NonRemote }
      { FriendlyName = DeviceFriendlyName "Badrum - Taklampa"
        Room = Bathroom
        Bulb = IkeaBulb
        ControlledWithRemote = NonRemote }
      { FriendlyName = DeviceFriendlyName "Sovrum - Nattduksbordlampa"
        Room = Bedroom
        Bulb = IkeaBulb
        ControlledWithRemote = RemoteLeft } ]

let remoteControlFriendlyName = DeviceFriendlyName "Fjärrkontroll"

type internal State =
    | On
    | Off

type internal Brightness = Brightness of int

type internal Color =
    | ColorByCoordinates of float * float
    | ColorByTemperature of int

type internal LightState =
    { State: State
      Brightness: Brightness
      Color: Color }
