module internal NightLight.Core.PartsOfDay

open System

type PartOfDay =
    | Day
    | Night

let private startOfDay = TimeSpan.FromHours 6
let private endOfDay = TimeSpan.FromHours 20.5

let getPartOfDay (dateTime: DateTime) =
    match dateTime with
    | _ when dateTime.TimeOfDay >= startOfDay && dateTime.TimeOfDay < endOfDay -> Day
    | _ -> Night

let getAlarmWeight (dateTime: DateTime) =
    let currentTime = dateTime.TimeOfDay
    let alarmEnd = startOfDay + TimeSpan(0, 15, 0)
    let totalSeconds = (alarmEnd - startOfDay).TotalSeconds

    if startOfDay <= currentTime && currentTime <= alarmEnd then
        let elapsedSeconds = (currentTime - startOfDay).TotalSeconds
        elapsedSeconds / totalSeconds
    else
        1.0
