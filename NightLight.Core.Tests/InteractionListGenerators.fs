module NightLight.Core.Tests.InteractionListGenerators

open System
open NightLight.Core.Models
open NightLight.Core.Tests.InteractionListHelpers
open FsCheck.FSharp

let private genTimeChanged =
    let genDate = ArbMap.defaults |> ArbMap.generate<DateTime> |> Gen.map _.Date

    let genTimeSpanBetween (l: TimeSpan) (h: TimeSpan) =
        Gen.choose (int l.TotalSeconds, int h.TotalSeconds)
        |> Gen.map int64
        |> Gen.map TimeSpan.FromSeconds

    let (+) = Gen.map2 (+)

    let genDayBoundaryDateTime =
        genDate
        + Gen.elements [ startOfDay; endOfDay ]
        + genTimeSpanBetween (-TimeSpan.FromMinutes 20.0) (TimeSpan.FromMinutes 20.0)

    let genStartOfDayDateTime =
        genDate
        + Gen.constant startOfDay
        + genTimeSpanBetween TimeSpan.Zero (TimeSpan.FromMinutes 20.0)

    let genAnyDateTime = ArbMap.defaults |> ArbMap.generate<DateTime>

    Gen.frequency [ 4, genStartOfDayDateTime; 2, genDayBoundaryDateTime; 1, genAnyDateTime ]
    |> Gen.map Interaction.TimeChanged

let private genHumanInteraction =
    Gen.elements lights
    |> Gen.bind (fun light -> Gen.elements [ LightPoweredOn light; LightPoweredOff light ])
    |> Gen.map Interaction.HumanInteraction

let private genRemoteInteraction =
    Gen.elements [ RemotePressedOnButton; RemotePressedOffButton; RemotePressedLeftButton ]
    |> Gen.map RemoteInteraction

let private genInteraction =
    Gen.frequency [ 4, genTimeChanged; 1, genHumanInteraction; 1, genRemoteInteraction ]

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
