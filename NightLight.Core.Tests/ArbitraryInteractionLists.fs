module NightLight.Core.Tests.ArbitraryInteractionLists

open System
open FsCheck.FSharp
open NightLight.Core.Tests.InteractionListGenerators

let private isDay (time: DateTime) =
    time.TimeOfDay >= TimeSpan.FromHours 5.5
    && time.TimeOfDay < TimeSpan.FromHours 20.5

type ArbitraryInteractionsListThatEndsDuringTheDay =
    static member InteractionsList() =
        ArbMap.defaults
        |> ArbMap.generate<DateTime>
        |> Gen.filter isDay
        |> Gen.bind genInteractionsListThatEndsAtTime
        |> Arb.fromGen

type ArbitraryInteractionsListThatEndsDuringTheNight =
    static member InteractionsList() =
        ArbMap.defaults
        |> ArbMap.generate<DateTime>
        |> Gen.filter (not << isDay)
        |> Gen.bind genInteractionsListThatEndsAtTime
        |> Arb.fromGen
