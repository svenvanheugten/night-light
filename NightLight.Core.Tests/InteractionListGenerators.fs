module NightLight.Core.Tests.InteractionListGenerators

open System
open FsCheck.FSharp
open NightLight.Core.Models

let private genTimeChangedInteraction =
    ArbMap.defaults |> ArbMap.generate<DateTime> |> Gen.map Interaction.TimeChanged

let private genHumanInteraction =
    Gen.elements lights
    |> Gen.bind (fun light ->
        [ LightTurnedOn light; LightTurnedOff light ]
        |> Gen.elements
        |> Gen.map Interaction.HumanInteraction)

let private genInteraction =
    Gen.oneof [ genTimeChangedInteraction; genHumanInteraction ]

let private genInteractionsListThatStartsWithTimeChange =
    gen {
        let! firstInteraction = genTimeChangedInteraction
        let! remainingInteractions = Gen.listOf genInteraction
        return firstInteraction :: remainingInteractions
    }

let private genInteractionsListWhere condition =
    Gen.listOf (genInteraction |> Gen.filter condition)

let genInteractionsListThatEndsAtTime time =
    let genTrivialList = Gen.constant <| List.singleton (Interaction.TimeChanged time)

    let genNonTrivialList =
        gen {
            let! before = genInteractionsListThatStartsWithTimeChange
            let interactionThatSetsEndTime = Interaction.TimeChanged time
            let! after = genInteractionsListWhere (not << _.IsTimeChanged)
            return before @ interactionThatSetsEndTime :: after
        }

    Gen.frequency [ 1, genTrivialList; 9, genNonTrivialList ]
