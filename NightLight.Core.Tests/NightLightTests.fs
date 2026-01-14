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
        concatGens
            [ genRandomInteractions light
              genTimeChangedToRandomDayTime |> Gen.map List.singleton
              genRandomInteractionsExcept light isTimeChangedToAnyNightTime ]
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
        concatGens
            [ genRandomInteractions light
              genTimeChangedToRandomNightTime |> Gen.map List.singleton
              genRandomInteractionsExcept light isTimeChangedToAnyDayTime ]
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
        genRandomInteractions light
        |> ensureStartsWithTimeChanged
        |> ensureLightHasPower light
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOn

    [<Property(Arbitrary = [| typeof<ArbitraryRemotelyControlledLight> |])>]
    let ``All remote controlled lights with power should be on if the remote was never used`` (light: Light) =
        genRandomInteractionsExcept light _.IsRemoteInteraction
        |> ensureStartsWithTimeChanged
        |> ensureLightHasPower light
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOn

    [<Property(Arbitrary = [| typeof<ArbitraryRemotelyControlledLight> |])>]
    let ``After pressing 'On' on the remote, if the remote isn't used again, all remotely controlled lights with power should be on``
        (light: Light)
        =
        concatGens
            [ genRandomInteractions light
              RemoteInteraction RemotePressedOnButton |> List.singleton |> Gen.constant
              genRandomInteractionsExcept light _.IsRemoteInteraction ]
        |> ensureStartsWithTimeChanged
        |> ensureLightHasPower light
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOn

    [<Property(Arbitrary = [| typeof<ArbitraryRemotelyControlledLight> |])>]
    let ``After a new day starts, if the remote isn't used, all remotely controlled lights with power should be on``
        (light: Light)
        =
        concatGens
            [ genRandomInteractions light
              genTimeChangedToRandomNightTime |> Gen.map List.singleton
              genRandomInteractionsExcept light isTimeChangedToAnyDayTime
              genTimeChangedToRandomDayTime |> Gen.map List.singleton
              genRandomInteractionsExcept light _.IsRemoteInteraction ]
        |> ensureStartsWithTimeChanged
        |> ensureLightHasPower light
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOn

    [<Property(Arbitrary = [| typeof<ArbitraryRemotelyControlledLight> |])>]
    let ``After pressing 'Off' on the remote, if the remote isn't used again and a new day doesn't start, all remotely controlled lights should be off``
        (light: Light)
        =
        concatGens
            [ genRandomInteractions light
              RemoteInteraction RemotePressedOffButton |> List.singleton |> Gen.constant
              genRandomInteractionsExcept light (fun interaction ->
                  interaction.IsRemoteInteraction || interaction |> isTimeChangedToAnyDayTime) ]
        |> ensureStartsWithTimeChanged
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOff

    [<Property(Arbitrary = [| typeof<ArbitraryLeftRemotelyControlledLight> |])>]
    let ``After pressing 'Left' on the remote, if the remote isn't used again, all left-side remotely controlled lights with power should be on``
        (light: Light)
        =
        concatGens
            [ genRandomInteractions light
              RemoteInteraction RemotePressedLeftButton |> List.singleton |> Gen.constant
              genRandomInteractionsExcept light _.IsRemoteInteraction ]
        |> ensureStartsWithTimeChanged
        |> ensureLightHasPower light
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOn

    [<Property(Arbitrary = [| typeof<ArbitraryRightRemotelyControlledLight> |])>]
    let ``After pressing 'Left' on the remote, if the remote isn't used again and a new day doesn't start, all right-side remotely controlled lights should be off``
        (light: Light)
        =
        concatGens
            [ genRandomInteractions light
              RemoteInteraction RemotePressedLeftButton |> List.singleton |> Gen.constant
              genRandomInteractionsExcept light (fun interaction ->
                  interaction.IsRemoteInteraction || interaction |> isTimeChangedToAnyDayTime) ]
        |> ensureStartsWithTimeChanged
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOff
