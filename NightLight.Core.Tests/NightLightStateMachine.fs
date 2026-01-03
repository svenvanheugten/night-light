namespace NightLight.Core.Tests

open System
open System.Collections.Generic
open FsToolkit.ErrorHandling
open NightLight.Core.Models
open NightLight.Core.Core

type NightLightStateMachine(now: DateTime) =
    let mutable state = { Time = now }

    let transmittedCommands = new List<Message>()

    let sendEvent event =
        result {
            let! newState, commands = onEventReceived state event
            state <- newState
            transmittedCommands.AddRange commands
        }

    member _.TransmittedCommands = transmittedCommands.AsReadOnly()

    member _.SendMessage message =
        ReceivedZigbeeEvent message |> sendEvent

    member _.ChangeTime time = TimeChanged time |> sendEvent

    member _.ClearTransmittedCommands() = transmittedCommands.Clear()
