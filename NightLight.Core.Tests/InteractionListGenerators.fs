module NightLight.Core.Tests.InteractionListGenerators

open System
open NightLight.Core.Models
open FsCheck
open FsCheck.FSharp

let private genTimeChanged =
    ArbMap.defaults |> ArbMap.generate<DateTime> |> Gen.map Interaction.TimeChanged

let private genHumanInteraction =
    Gen.elements lights
    |> Gen.bind (fun light -> Gen.elements [ LightPoweredOn light; LightPoweredOff light ])
    |> Gen.map Interaction.HumanInteraction

let private genRemoteInteraction =
    Gen.elements [ RemotePressedOnButton; RemotePressedOffButton; RemotePressedLeftButton ]
    |> Gen.map RemoteInteraction

let private genInteraction =
    Gen.oneof [ genTimeChanged; genHumanInteraction; genRemoteInteraction ]

let private genInteractions = genInteraction |> Gen.listOf

let private ensureStartsWithTimeChanged (genInteractions: Gen<Interaction list>) =
    genInteractions
    |> Gen.bind (fun interactions ->
        match interactions with
        | Interaction.TimeChanged _ :: _ -> Gen.constant interactions
        | _ -> genTimeChanged |> Gen.map (fun tc -> tc :: interactions))

type ArbitraryInteractions() =
    static member Interactions() =
        genInteractions |> ensureStartsWithTimeChanged |> Arb.fromGen
