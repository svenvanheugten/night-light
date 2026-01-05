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

let private genInteractionsListThatStartsWithTimeChanged =
    gen {
        let! firstInteraction = genTimeChangedInteraction
        let! remainingInteractions = Gen.listOf genInteraction
        return firstInteraction :: remainingInteractions
    }

let private genInteractionListContaining containingInteraction afterFilter =
    gen {
        let genNonTrivialList =
            gen {
                let! before = genInteractionsListThatStartsWithTimeChanged
                let! after = Gen.listOf (genInteraction |> Gen.filter afterFilter)
                return before @ containingInteraction :: after
            }

        return!
            match containingInteraction with
            | Interaction.TimeChanged _ ->
                let genTrivialList = Gen.constant <| List.singleton containingInteraction
                Gen.frequency [ 1, genTrivialList; 9, genNonTrivialList ]
            | _ -> genNonTrivialList
    }

let genInteractionListThatEndsAtTime time =
    genInteractionListContaining (Interaction.TimeChanged time) (not << _.IsTimeChanged)
