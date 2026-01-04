namespace NightLight.Core.Tests

open System
open System.Collections.Generic
open FsToolkit.ErrorHandling
open NightLight.Core.Models
open NightLight.Core.Core

type NightLightStateMachine(now: DateTime) =
    let mutable state = State now

    let transmittedCommands = new List<Message>()

    let assertIsOk (result: Result<unit, 'a>) : unit =
        match result with
        | Ok() -> ()
        | Error error -> failwith $"Expected Ok, got Error {error}"

    let sendEvent event =
        result {
            let! newState, commands = state.OnEventReceived event
            state <- newState
            transmittedCommands.AddRange commands
        }
        |> assertIsOk

    member _.TransmittedCommands = transmittedCommands.AsReadOnly()

    member _.SendMessage message =
        ReceivedZigbeeEvent message |> sendEvent

    member _.ChangeTime time = TimeChanged time |> sendEvent

    member _.ClearTransmittedCommands() = transmittedCommands.Clear()
