module NightLight.Core.Tests.TimeChangedGenerators

open System
open FsCheck.FSharp

type PartOfDay =
    | Day
    | Night

let getPartOfDay (dateTime: DateTime) =
    match dateTime with
    | _ when
        dateTime.TimeOfDay >= TimeSpan.FromHours 6
        && dateTime.TimeOfDay < TimeSpan.FromHours 20.5
        ->
        Day
    | _ -> Night

let genTimeChanged =
    ArbMap.defaults |> ArbMap.generate<DateTime> |> Gen.map Interaction.TimeChanged

let isTimeChangedToPartOfDay partOfDay interaction =
    match interaction with
    | TimeChanged time when getPartOfDay time = partOfDay -> true
    | _ -> false

let genTimeChangedToPartOfDay (partOfDay: PartOfDay) =
    genTimeChanged |> Gen.filter (isTimeChangedToPartOfDay partOfDay)
