module NightLight.Core.Tests.InteractionListGenerators

open FsCheck.FSharp
open NightLight.Core.Models
open NightLight.Core.Tests.TimeChangedGenerators
open FsCheck

let private genHumanInteraction biasTowardsLight =
    Gen.oneof [ Gen.constant biasTowardsLight; Gen.elements lights ]
    |> Gen.bind (fun light -> Gen.elements [ LightPoweredOn light; LightPoweredOff light ])
    |> Gen.map Interaction.HumanInteraction

let private genRemoteInteraction =
    Gen.elements [ RemotePressedOnButton; RemotePressedOffButton ]
    |> Gen.map RemoteInteraction

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

let ensureLightHasPower (light: Light) (genInteractions: Gen<Interaction list>) =
    genInteractions
    |> Gen.map (fun interactions ->
        let lightHasPower =
            interactions
            |> Seq.choose (fun interaction ->
                match interaction with
                | HumanInteraction(LightPoweredOff l) when l = light -> Some false
                | HumanInteraction(LightPoweredOn l) when l = light -> Some true
                | _ -> None)
            |> Seq.tryLast
            |> Option.defaultValue false

        if lightHasPower then
            interactions
        else
            interactions @ [ HumanInteraction(LightPoweredOn light) ])
