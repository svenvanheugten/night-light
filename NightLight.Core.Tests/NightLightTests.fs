namespace NightLight.Core.Tests

open NightLight.Core.Core
open NightLight.Core.Tests.GenHelpers
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

    let filterToLightsWithPower interactions lights =
        lights |> Seq.filter (fst >> doesLightHavePowerAfter interactions)

    [<Property>]
    let ``All lights that are on should be white or yellow during the day`` () =
        concatGens
            [ Gen.bind genInteractionListThatStartsWithTimeChangedAndEndsWith genTimeChangedToDay
              genInteractionListExcept isTimeChangedToNight ]
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions

            fakeHome.ForAllLightsThatAreOn(fun (_, _, color) -> color = White || color = Yellow)
            |> Prop.trivial (fakeHome.LightStates |> Seq.filter (snd >> _.IsOn) |> Seq.isEmpty)

    [<Property>]
    let ``All lights that are on should be red during the night`` () =
        concatGens
            [ Gen.bind genInteractionListThatStartsWithTimeChangedAndEndsWith genTimeChangedToNight
              genInteractionListExcept isTimeChangedToDay ]
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions

            fakeHome.ForAllLightsThatAreOn(fun (_, _, color) -> color = Red)
            |> Prop.trivial (fakeHome.LightStates |> Seq.filter (snd >> _.IsOn) |> Seq.isEmpty)

    [<Property>]
    let ``After pressing 'On' on the remote, all lights that have power should be on as long as the 'Off' button isn't pressed``
        ()
        =
        concatGens
            [ genInteractionListThatStartsWithTimeChangedAndEndsWith (HumanInteraction RemotePressedOnButton)
              genInteractionListExcept ((=) (HumanInteraction RemotePressedOffButton)) ]
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            let lightsWithPower = fakeHome.LightStates |> filterToLightsWithPower interactions

            lightsWithPower
            |> Seq.map snd
            |> Seq.forall _.IsOn
            |> Prop.trivial (Seq.isEmpty lightsWithPower)

    [<Property>]
    let ``After pressing 'Off' on the remote, all lights that have power should be on as long as the 'On' button isn't pressed and a new day doesn't start``
        ()
        =
        concatGens
            [ genInteractionListThatStartsWithTimeChangedAndEndsWith (HumanInteraction RemotePressedOffButton)
              genInteractionListExcept (fun interaction ->
                  interaction = HumanInteraction RemotePressedOnButton
                  || interaction |> isTimeChangedToDay) ]
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            let lightsWithPower = fakeHome.LightStates |> filterToLightsWithPower interactions

            fakeHome.ForAllRemotelyControlledLights(fun (_, state) -> state = Off)
            |> Prop.trivial (Seq.isEmpty lightsWithPower)

    [<Property>]
    let ``After a new day starts, all lights that have power should be on as long as the 'Off' button isn't pressed``
        ()
        =
        let genInitialTransitionToDay =
            concatGens
                [ Gen.bind genInteractionListThatStartsWithTimeChangedAndEndsWith genTimeChangedToNight
                  genInteractionListExcept isTimeChangedToDay
                  Gen.map List.singleton genTimeChangedToDay ]

        concatGens
            [ genInitialTransitionToDay
              genInteractionListExcept ((=) (HumanInteraction RemotePressedOffButton)) ]
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            let lightsWithPower = fakeHome.LightStates |> filterToLightsWithPower interactions

            lightsWithPower
            |> Seq.map snd
            |> Seq.forall _.IsOn
            |> Prop.trivial (Seq.isEmpty lightsWithPower)
