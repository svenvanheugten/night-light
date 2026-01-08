module NightLight.Core.Tests.TimeChangedGenerators

open System
open FsCheck.FSharp

let private isDay (time: DateTime) =
    time.TimeOfDay >= TimeSpan.FromHours 5.5
    && time.TimeOfDay < TimeSpan.FromHours 20.5

let private isTimeChangedMeetingCondition condition interaction =
    match interaction with
    | TimeChanged time when condition time -> true
    | _ -> false

let isTimeChangedToAnyDayTime = isTimeChangedMeetingCondition isDay

let isTimeChangedToAnyNightTime = isTimeChangedMeetingCondition (not << isDay)

let genTimeChanged =
    ArbMap.defaults |> ArbMap.generate<DateTime> |> Gen.map Interaction.TimeChanged

let genTimeChangedToRandomDayTime =
    genTimeChanged |> Gen.filter isTimeChangedToAnyDayTime

let genTimeChangedToRandomNightTime =
    genTimeChanged |> Gen.filter isTimeChangedToAnyNightTime
