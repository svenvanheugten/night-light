module NightLight.Core.Tests.InteractionListGenerators

open System
open NightLight.Core.Models
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

type ArbitraryInteractions() =
    static member Interactions() =
        let gen =
            genInteraction
            |> Gen.listOf
            |> Gen.bind (fun interactions ->
                match interactions with
                | Interaction.TimeChanged _ :: _ -> Gen.constant interactions
                | _ -> genTimeChanged |> Gen.map (fun tc -> tc :: interactions))

        let removeFromFrontAfterFirst (lst: 'a list) : seq<'a list> =
            Seq.unfold
                (fun current ->
                    match current with
                    | [] -> None
                    | [ _ ] -> None
                    | first :: _ :: rest ->
                        let next = first :: rest
                        Some(next, next))
                lst

        Arb.fromGenShrink (gen, removeFromFrontAfterFirst)
