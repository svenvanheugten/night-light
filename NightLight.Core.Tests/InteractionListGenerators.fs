module NightLight.Core.Tests.InteractionListGenerators

open FsCheck.FSharp
open NightLight.Core.Models
open NightLight.Core.Tests.TimeChangedGenerators
open FsCheck

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

let genRandomInteractionsExcept biasTowardsLight disqualifier =
    genInteraction biasTowardsLight
    |> Gen.filter (not << disqualifier)
    |> Gen.listOf

let genRandomInteractions biasTowardsLight =
    genRandomInteractionsExcept biasTowardsLight (fun _ -> false)

let ensureStartsWithTimeChanged (genInteractions: Gen<Interaction list>) =
    genInteractions
    |> Gen.bind (fun interactions ->
        match interactions with
        | Interaction.TimeChanged _ :: _ -> Gen.constant interactions
        | _ -> genTimeChanged |> Gen.map (fun tc -> tc :: interactions))
