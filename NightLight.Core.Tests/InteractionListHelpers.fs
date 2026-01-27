module NightLight.Core.Tests.InteractionListHelpers

open System

type PartOfDay =
    | Day
    | Night

let startOfDay = TimeSpan.FromHours 5.25
let endOfAlarm = TimeSpan.FromHours 5.50
let endOfDay = TimeSpan.FromHours 20.5

let getPartOfDay (dateTime: DateTime) =
    match dateTime with
    | _ when dateTime.TimeOfDay >= startOfDay && dateTime.TimeOfDay < endOfDay -> Day
    | _ -> Night

let getTimeAfterInteractions interactions =
    interactions
    |> Seq.choose (fun interaction ->
        match interaction with
        | Interaction.TimeChanged time -> Some time
        | _ -> None)
    |> Seq.last

let getPartOfDayAfterInteractions interactions =
    interactions |> getTimeAfterInteractions |> getPartOfDay

let doesLightHavePowerAfterInteractions light interactions =
    interactions
    |> Seq.choose (fun interaction ->
        match interaction with
        | HumanInteraction(LightPoweredOff l) when l = light -> Some false
        | HumanInteraction(LightPoweredOn l) when l = light -> Some true
        | _ -> None)
    |> Seq.tryLast
    |> Option.defaultValue false

let tryGetLastRemoteInteraction interactions =
    interactions
    |> Seq.indexed
    |> Seq.choose (fun interaction ->
        match interaction with
        | index, Interaction.RemoteInteraction remoteInteraction -> Some(index, remoteInteraction)
        | _ -> None)
    |> Seq.tryLast

let hasNewDayStartedSince interactions maybeSince =
    let maybeLastNightToDayTransitionIndex =
        interactions
        |> Seq.indexed
        |> Seq.choose (fun (index, interaction) ->
            match interaction with
            | Interaction.TimeChanged time -> Some(index, getPartOfDay time)
            | _ -> None)
        |> Seq.pairwise
        |> Seq.tryFindBack (fun ((_, v1), (_, v2)) -> v1 = Night && v2 = Day)
        |> Option.map (fun ((_, _), (i2, _)) -> i2)

    match maybeLastNightToDayTransitionIndex, maybeSince with
    | Some lastNightToDayTransitionIndex, Some(sinceIndex, _) -> sinceIndex < lastNightToDayTransitionIndex
    | Some _, None -> true
    | None, _ -> false
