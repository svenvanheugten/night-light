module NightLight.Core.Tests.TimeChangedGenerators

open System
open FsCheck.FSharp

let private isDay (time: DateTime) =
    time.TimeOfDay >= TimeSpan.FromHours 5.5
    && time.TimeOfDay < TimeSpan.FromHours 20.5

let genTimeChangedToDay =
    ArbMap.defaults
    |> ArbMap.generate<DateTime>
    |> Gen.filter isDay
    |> Gen.map Interaction.TimeChanged

let genTimeChangedToNight =
    ArbMap.defaults
    |> ArbMap.generate<DateTime>
    |> Gen.filter (not << isDay)
    |> Gen.map Interaction.TimeChanged
