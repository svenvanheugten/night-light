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

let genRandomInteractionsExcept biasTowardsLight disqualifier =
    genInteraction biasTowardsLight
    |> Gen.filter (not << disqualifier)
    |> Gen.listOf

let genInitialInteractionsExcept biasTowardsLight disqualifier =
    [ genTimeChanged |> Gen.map List.singleton
      genRandomInteractionsExcept biasTowardsLight disqualifier ]
    |> concatGens
