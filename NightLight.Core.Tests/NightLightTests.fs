namespace NightLight.Core.Tests

open NightLight.Core.Core
open NightLight.Core.Tests.GenHelpers
open NightLight.Core.Tests.TimeChangedGenerators
open NightLight.Core.Tests.InteractionListGenerators
open NightLight.Core.Models
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

    [<Property(Arbitrary = [| typeof<ArbitraryLight> |])>]
    let ``All lights should be either off, white or yellow during the day`` (light: Light) =
        genInteractions light
        |> ensurePartOfDayIs Day
        |> ensureStartsWithTimeChanged
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions

            fakeHome.LightShouldHaveState light (function
                | Off -> true
                | On(_, color) -> color = White || color = Yellow)

    [<Property(Arbitrary = [| typeof<ArbitraryLight> |])>]
    let ``All lights should be either off or red during the night`` (light: Light) =
        genInteractions light
        |> ensurePartOfDayIs Night
        |> ensureStartsWithTimeChanged
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions

            fakeHome.LightShouldHaveState light (function
                | Off -> true
                | On(_, color) -> color = Red)

    [<Property(Arbitrary = [| typeof<ArbitraryNonRemotelyControlledLight> |])>]
    let ``All non-remotely controlled lights with power should be on`` (light: Light) =
        genInteractions light
        |> ensureLightHasPower light
        |> ensureStartsWithTimeChanged
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOn

    [<Property(Arbitrary = [| typeof<ArbitraryRemotelyControlledLight> |])>]
    let ``If the remote was never used, all remote controlled lights with power should be on`` (light: Light) =
        genInteractionsExcept light _.IsRemoteInteraction
        |> ensureLightHasPower light
        |> ensureStartsWithTimeChanged
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOn

    [<Property(Arbitrary = [| typeof<ArbitraryRemotelyControlledLight> |])>]
    let ``If the last button that was pressed on the remote is 'On', all remotely controlled lights with power should be on``
        (light: Light)
        =
        genInteractions light
        |> ensureLastRemoteInteractionIs RemotePressedOnButton
        |> ensureLightHasPower light
        |> ensureStartsWithTimeChanged
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOn

    [<Property(Arbitrary = [| typeof<ArbitraryRemotelyControlledLight> |])>]
    let ``If a new day has started and the remote hasn't been used yet, all remotely controlled lights with power should be on``
        (light: Light)
        =
        concatGens
            [ genInteractions light |> ensurePartOfDayIs Night
              genTimeChangedToPartOfDay Day |> Gen.map List.singleton
              genInteractionsExcept light _.IsRemoteInteraction ]
        |> ensureStartsWithTimeChanged
        |> ensureLightHasPower light
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOn

    [<Property(Arbitrary = [| typeof<ArbitraryRemotelyControlledLight> |])>]
    let ``If the last button that was pressed on the remote is 'Off' and a new day hasn't started yet, all remotely controlled lights should be off``
        (light: Light)
        =
        concatGens
            [ genInteractions light
              RemoteInteraction RemotePressedOffButton |> List.singleton |> Gen.constant
              genInteractionsExcept light (fun interaction ->
                  interaction.IsRemoteInteraction || interaction |> isTimeChangedToPartOfDay Day) ]
        |> ensureStartsWithTimeChanged
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOff

    [<Property(Arbitrary = [| typeof<ArbitraryLeftRemotelyControlledLight> |])>]
    let ``If the last button that was pressed on the remote is 'Left', all left-side remotely controlled lights with power should be on``
        (light: Light)
        =
        genInteractions light
        |> ensureLastRemoteInteractionIs RemotePressedLeftButton
        |> ensureStartsWithTimeChanged
        |> ensureLightHasPower light
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOn

    [<Property(Arbitrary = [| typeof<ArbitraryRightRemotelyControlledLight> |])>]
    let ``If the last button that was pressed on the remote is 'Left' and a new day hasn't started yet, all right-side remotely controlled lights should be off``
        (light: Light)
        =
        concatGens
            [ genInteractions light
              RemoteInteraction RemotePressedLeftButton |> List.singleton |> Gen.constant
              genInteractionsExcept light (fun interaction ->
                  interaction.IsRemoteInteraction || interaction |> isTimeChangedToPartOfDay Day) ]
        |> ensureStartsWithTimeChanged
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOff
