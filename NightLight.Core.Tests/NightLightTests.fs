namespace NightLight.Core.Tests

open NightLight.Core.Core
open NightLight.Core.Tests.ArbitraryInteractionLists
open FsCheck.Xunit

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

    [<Property(Arbitrary = [| typeof<ArbitraryInteractionListThatEndsDuringTheDay> |])>]
    let ``All lights that are on should be white or yellow during the day`` (interactions: Interaction list) =
        let fakeHome = createFakeHomeWithNightLightAndInteract interactions
        fakeHome.ForAllLightsThatAreOn(fun (_, _, color) -> color = White || color = Yellow)

    [<Property(Arbitrary = [| typeof<ArbitraryInteractionListThatEndsDuringTheNight> |])>]
    let ``All lights that are on should be red during the night`` (interactions: Interaction list) =
        let fakeHome = createFakeHomeWithNightLightAndInteract interactions
        fakeHome.ForAllLightsThatAreOn(fun (_, _, color) -> color = Red)
