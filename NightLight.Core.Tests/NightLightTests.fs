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

    let doesLightHavePowerAfter interactions light =
        interactions
        |> Seq.choose (fun interaction ->
            match interaction with
            | HumanInteraction(LightPoweredOff l) when l = light -> Some false
            | HumanInteraction(LightPoweredOn l) when l = light -> Some true
            | _ -> None)
        |> Seq.tryLast
        |> Option.defaultValue false

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
    let ``After pressing 'On' on the remote, the remotely controlled lights that have power should be on until they are powered off or 'Off' is pressed``
        ()
        =
        genInteractionListContaining (HumanInteraction RemotePressedOnButton) (function
            | HumanInteraction RemotePressedOffButton -> true
            | HumanInteraction(LightPoweredOff l) when l.ControlledWithRemote -> true
            | _ -> false)
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions

            fakeHome.LightStates
            |> Seq.filter (fst >> doesLightHavePowerAfter interactions)
            |> Seq.map snd
            |> Seq.forall _.IsOn

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
