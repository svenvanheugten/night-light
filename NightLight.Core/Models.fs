module NightLight.Models

open System

type State = { Time: DateTime }

type Event =
    | ReceivedZigbeeEvent of payload: string
    | TimeChanged of DateTime

type Message = Message of Topic: string * Payload: string

type ParseZigbeeEventError =
    | InvalidJson
    | MissingTypeField
    | MissingDataField
    | MissingFriendlyNameField
    | InvalidTypeField
    | InvalidFriendlyNameField
    | UnknownType

type ParseEventError = ParseZigbeeEventError of ParseZigbeeEventError
