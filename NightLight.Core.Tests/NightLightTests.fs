namespace NightLight.Core.Tests

open NightLight.Core.Core
open NightLight.Core.Tests.TimeChangedGenerators
open NightLight.Core.Tests.InteractionListGenerators
open FsCheck.Xunit
open FsCheck.FSharp

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
    let ``All lights that are on should be white or yellow during the day`` () =
        genTimeChangedToDay
        |> Gen.bind (fun timeChangedToDay -> genInteractionListContaining timeChangedToDay _.IsTimeChanged)
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.ForAllLightsThatAreOn(fun (_, _, color) -> color = White || color = Yellow)

    [<Property>]
    let ``All lights that are on should be red during the night`` () =
        genTimeChangedToNight
        |> Gen.bind (fun timeChangedToNight -> genInteractionListContaining timeChangedToNight _.IsTimeChanged)
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.ForAllLightsThatAreOn(fun (_, _, color) -> color = Red)

    [<Property>]
    let ``After pressing 'Off' on the remote, the remotely controlled lights should stay off until 'On' is pressed``
        ()
        =
        genInteractionListContaining
            (HumanInteraction RemotePressedOffButton)
            ((=) (HumanInteraction RemotePressedOnButton))
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.ForAllRemotelyControlledLights(fun (_, state) -> state = Off)
