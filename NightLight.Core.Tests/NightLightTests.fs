namespace NightLight.Core.Tests

open System
open NightLight.Core.Core
open FsCheck.Xunit
open FsCheck.FSharp

module InteractionsHelpers =
    let getTimeAfter interactions =
        interactions
        |> Seq.choose (fun interaction ->
            match interaction with
            | TimeChanged time -> Some time
            | _ -> None)
        |> Seq.tryLast
        |> function
            | Some time -> time
            | None -> failwith "Time wasn't changed"

    let isDayAfter interactions =
        let time = getTimeAfter interactions

        time.TimeOfDay >= TimeSpan.FromHours 5.5
        && time.TimeOfDay < TimeSpan.FromHours 20.5

    let isNightAfter = not << isDayAfter

[<Properties(Arbitrary = [| typeof<Arbitraries> |])>]
type NightLightTests() =
    let createFakeHomeWithNightLightAndInteract (interactions: Interaction list) =
        let mutable nightLightStateMachine = NightLightStateMachine()

        let fakeHome = FakeHome()

        fakeHome.OnEventPublished.Add(fun event ->
            match event |> nightLightStateMachine.OnEventReceived with
            | Ok(newState, commands) ->
                commands |> Seq.iter fakeHome.ProcessCommand
                nightLightStateMachine <- newState
            | Error error -> failwith $"Unexpected error {error}")

        fakeHome.Interact interactions

        fakeHome

    [<Property>]
    let ``Brightness should always be under 255`` (interactions: Interaction list) =
        let fakeHome = createFakeHomeWithNightLightAndInteract interactions
        fakeHome.ForAllLightsThatAreOn(fun (_, brightness, _) -> brightness < 255uy)

    [<Property>]
    let ``Lights should be red during the night`` (interactions: Interaction list) =
        let fakeHome = createFakeHomeWithNightLightAndInteract interactions

        InteractionsHelpers.isNightAfter interactions
        ==> fakeHome.ForAllLightsThatAreOn(fun (_, _, color) -> color = Red)

    [<Property>]
    let ``Lights should be white or yellow during the day`` (interactions: Interaction list) =
        let fakeHome = createFakeHomeWithNightLightAndInteract interactions

        InteractionsHelpers.isDayAfter interactions
        ==> fakeHome.ForAllLightsThatAreOn(fun (_, _, color) -> color = White || color = Yellow)
