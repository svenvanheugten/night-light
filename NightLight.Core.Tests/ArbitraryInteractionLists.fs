module NightLight.Core.Tests.ArbitraryInteractionLists

open System
open FsCheck.FSharp
open NightLight.Core.Tests.InteractionListGenerators
open NightLight.Core.Models

let private isDay (time: DateTime) =
    time.TimeOfDay >= TimeSpan.FromHours 5.5
    && time.TimeOfDay < TimeSpan.FromHours 20.5

type ArbitraryInteractionListThatEndsDuringTheDay =
    static member InteractionsList() =
        ArbMap.defaults
        |> ArbMap.generate<DateTime>
        |> Gen.filter isDay
        |> Gen.bind genInteractionListThatEndsAtTime
        |> Arb.fromGen

type ArbitraryInteractionListThatEndsDuringTheNight =
    static member InteractionsList() =
        ArbMap.defaults
        |> ArbMap.generate<DateTime>
        |> Gen.filter (not << isDay)
        |> Gen.bind genInteractionListThatEndsAtTime
        |> Arb.fromGen
