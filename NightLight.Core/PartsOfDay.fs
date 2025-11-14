module NightLight.PartsOfDay

open System

type PartOfDay =
    | Day
    | Night

let getPartOfDay (dateTime: DateTime) =
    match dateTime with
    | _ when dateTime.TimeOfDay >= TimeSpan.FromHours 4.75 && dateTime.TimeOfDay < TimeSpan.FromHours 20.5 -> Day
    | _ -> Night
