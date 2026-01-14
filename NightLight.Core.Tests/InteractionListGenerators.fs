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
    Gen.elements [ RemotePressedOnButton; RemotePressedOffButton; RemotePressedLeftButton ]
    |> Gen.map RemoteInteraction

let private genInteraction biasTowardsLight =
    Gen.oneof [ genTimeChanged; genHumanInteraction biasTowardsLight; genRemoteInteraction ]

let genInteractionsExcept biasTowardsLight disqualifier =
    genInteraction biasTowardsLight
    |> Gen.filter (not << disqualifier)
    |> Gen.listOf

let genInteractions biasTowardsLight =
    genInteractionsExcept biasTowardsLight (fun _ -> false)

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

let ensurePartOfDayIs (desiredPartOfDay: PartOfDay) (genInteractions: Gen<Interaction list>) =
    genInteractions
    |> Gen.bind (fun interactions ->
        let maybeActualTime =
            interactions
            |> Seq.choose (fun interaction ->
                match interaction with
                | Interaction.TimeChanged time -> Some time
                | _ -> None)
            |> Seq.tryLast

        if maybeActualTime |> Option.map getPartOfDay = Some desiredPartOfDay then
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
