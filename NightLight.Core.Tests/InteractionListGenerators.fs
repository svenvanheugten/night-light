module NightLight.Core.Tests.InteractionListGenerators

open FsCheck.FSharp
open NightLight.Core.Models
open NightLight.Core.Tests.GenHelpers
open NightLight.Core.Tests.TimeChangedGenerators

let private genHumanInteraction biasTowardsLight =
    let genLightInteraction =
        Gen.oneof [ Gen.constant biasTowardsLight; Gen.elements lights ]
        |> Gen.bind (fun light -> Gen.elements [ LightPoweredOn light; LightPoweredOff light ])

    let genRemoteInteraction =
        Gen.elements [ RemotePressedOnButton; RemotePressedOffButton ]

    Gen.oneof [ genLightInteraction; genRemoteInteraction ]
    |> Gen.map Interaction.HumanInteraction

let private genInteraction biasTowardsLight =
    Gen.oneof [ genTimeChanged; genHumanInteraction biasTowardsLight ]

let genInitialInteractions biasTowardsLight =
    [ genTimeChanged |> Gen.map List.singleton
      Gen.listOf <| genInteraction biasTowardsLight ]
    |> concatGens

let genInitialInteractionsAndEndWith biasTowardsLight (endsWith: Interaction) =
    let genNonTrivialList =
        genInitialInteractions biasTowardsLight
        |> Gen.map (fun lst -> lst @ [ endsWith ])

    match endsWith with
    | Interaction.TimeChanged _ ->
        let genTrivialList = Gen.constant <| List.singleton endsWith
        Gen.frequency [ 1, genTrivialList; 9, genNonTrivialList ]
    | _ -> genNonTrivialList

let genInteractionsExcept biasTowardsLight disqualifier =
    genInteraction biasTowardsLight
    |> Gen.filter (not << disqualifier)
    |> Gen.listOf
