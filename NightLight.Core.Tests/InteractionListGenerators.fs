module NightLight.Core.Tests.InteractionListGenerators

open FsCheck.FSharp
open NightLight.Core.Models
open NightLight.Core.Tests.TimeChangedGenerators
open FsCheck

let private genHumanInteraction maybeBiasTowardsLight =
    match maybeBiasTowardsLight with
    | Some biasTowardsLight -> Gen.oneof [ Gen.constant biasTowardsLight; Gen.elements lights ]
    | None -> Gen.elements lights
    |> Gen.bind (fun light -> Gen.elements [ LightPoweredOn light; LightPoweredOff light ])
    |> Gen.map Interaction.HumanInteraction

let private genRemoteInteraction =
    Gen.elements [ RemotePressedOnButton; RemotePressedOffButton; RemotePressedLeftButton ]
    |> Gen.map RemoteInteraction

let private genInteraction maybeBiasTowardsLight =
    Gen.oneof
        [ genTimeChanged
          genHumanInteraction maybeBiasTowardsLight
          genRemoteInteraction ]

let genBiasedInteractionsExcept biasTowardsLight disqualifier =
    Some biasTowardsLight
    |> genInteraction
    |> Gen.filter (not << disqualifier)
    |> Gen.listOf

let genBiasedInteractions biasTowardsLight =
    genBiasedInteractionsExcept biasTowardsLight (fun _ -> false)

let genInteractions = genInteraction None |> Gen.listOf

let getPartOfDayAfterInteractions interactions =
    interactions
    |> Seq.choose (fun interaction ->
        match interaction with
        | Interaction.TimeChanged time -> Some time
        | _ -> None)
    |> Seq.last
    |> getPartOfDay

let doesLightHavePowerAfterInteractions light interactions =
    interactions
    |> Seq.choose (fun interaction ->
        match interaction with
        | HumanInteraction(LightPoweredOff l) when l = light -> Some false
        | HumanInteraction(LightPoweredOn l) when l = light -> Some true
        | _ -> None)
    |> Seq.tryLast
    |> Option.defaultValue false

let ensureStartsWithTimeChanged (genInteractions: Gen<Interaction list>) =
    genInteractions
    |> Gen.bind (fun interactions ->
        match interactions with
        | Interaction.TimeChanged _ :: _ -> Gen.constant interactions
        | _ -> genTimeChanged |> Gen.map (fun tc -> tc :: interactions))

let ensureLightHasPower (light: Light) (genInteractions: Gen<Interaction list>) =
    genInteractions
    |> Gen.map (fun interactions ->
        if doesLightHavePowerAfterInteractions light interactions then
            interactions
        else
            interactions @ [ HumanInteraction(LightPoweredOn light) ])

let ensurePartOfDayIs (desiredPartOfDay: PartOfDay) (genInteractions: Gen<Interaction list>) =
    genInteractions
    |> Gen.bind (fun interactions ->
        if getPartOfDayAfterInteractions interactions = desiredPartOfDay then
            Gen.constant interactions
        else
            desiredPartOfDay
            |> genTimeChangedToPartOfDay
            |> Gen.map (fun tc -> interactions @ [ tc ]))

let ensureLastRemoteInteractionIs
    (desiredLastRemoteInteraction: RemoteInteraction)
    (genInteractions: Gen<Interaction list>)
    =
    genInteractions
    |> Gen.map (fun interactions ->
        let maybeLastRemoteInteraction =
            interactions
            |> Seq.choose (fun interaction ->
                match interaction with
                | Interaction.RemoteInteraction remoteInteraction -> Some remoteInteraction
                | _ -> None)
            |> Seq.tryLast

        if maybeLastRemoteInteraction = Some desiredLastRemoteInteraction then
            interactions
        else
            interactions @ [ RemoteInteraction desiredLastRemoteInteraction ])

type ArbitraryInteractions() =
    static member Interactions() =
        genInteractions |> ensureStartsWithTimeChanged |> Arb.fromGen
