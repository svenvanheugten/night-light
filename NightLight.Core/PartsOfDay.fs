module internal NightLight.PartsOfDay

open System

type PartOfDay =
    | Day
    | Night

let getPartOfDay (dateTime: DateTime) =
    match dateTime with
    | _ when
        dateTime.TimeOfDay >= TimeSpan.FromHours 5.5
        && dateTime.TimeOfDay < TimeSpan.FromHours 20.5
        ->
        Day
    | _ -> Night
