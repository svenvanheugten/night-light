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

let isTimeChangedToDay = isTimeChangedMeetingCondition isDay

let isTimeChangedToNight = isTimeChangedMeetingCondition (not << isDay)

let genTimeChanged =
    ArbMap.defaults |> ArbMap.generate<DateTime> |> Gen.map Interaction.TimeChanged

let genTimeChangedToDay = genTimeChanged |> Gen.filter isTimeChangedToDay

let genTimeChangedToNight = genTimeChanged |> Gen.filter isTimeChangedToNight
