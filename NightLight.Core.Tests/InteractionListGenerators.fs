module NightLight.Core.Tests.InteractionListGenerators

open System
open FsCheck.FSharp
open NightLight.Core.Models

let private genTimeChangedInteraction =
    ArbMap.defaults |> ArbMap.generate<DateTime> |> Gen.map Interaction.TimeChanged

let private genHumanInteraction =
    let genLightInteraction =
        Gen.elements lights
        |> Gen.bind (fun light -> Gen.elements [ LightPoweredOn light; LightPoweredOff light ])

    let genRemoteInteraction =
        Gen.elements [ RemotePressedOnButton; RemotePressedOffButton ]

    Gen.oneof [ genLightInteraction; genRemoteInteraction ]
    |> Gen.map Interaction.HumanInteraction

let private genInteraction =
    Gen.oneof [ genTimeChangedInteraction; genHumanInteraction ]

let private genInteractionsListThatStartsWithTimeChanged =
    gen {
        let! firstInteraction = genTimeChangedInteraction
        let! remainingInteractions = Gen.listOf genInteraction
        return firstInteraction :: remainingInteractions
    }

let genInteractionListContaining containingInteraction disqualifiedAfter =
    gen {
        let genNonTrivialList =
            gen {
                let! before = genInteractionsListThatStartsWithTimeChanged
                let! after = Gen.listOf (genInteraction |> Gen.filter (not << disqualifiedAfter))
                return before @ containingInteraction :: after
            }

        return!
            match containingInteraction with
            | Interaction.TimeChanged _ ->
                let genTrivialList = Gen.constant <| List.singleton containingInteraction
                Gen.frequency [ 1, genTrivialList; 9, genNonTrivialList ]
            | _ -> genNonTrivialList
    }
