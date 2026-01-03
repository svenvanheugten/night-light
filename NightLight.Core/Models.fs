module NightLight.Core.Models

open System

type State = { Time: DateTime }

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

type ParseEventError = ParseZigbeeEventError of ParseZigbeeEventError
