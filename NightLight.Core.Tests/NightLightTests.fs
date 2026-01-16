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

    [<Property>]
    let ``All lights should either be off or have the right color`` () =
        genInteractions |> ensureStartsWithTimeChanged |> Arb.fromGen |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            let partOfDay = getPartOfDayAfterInteractions interactions

            fakeHome.LightStates
            |> Seq.forall (function
                | _, Off -> true
                | _, On(_, color) ->
                    match partOfDay with
                    | Day -> color = White || color = Yellow
                    | Night -> color = Red)
            |> Prop.classify (partOfDay = Day) "day"
            |> Prop.classify (partOfDay = Night) "night"
            |> Prop.trivial (fakeHome.LightsThatAreOn.Length = 0)
            |> Prop.collect $"{fakeHome.LightsThatAreOn.Length} light(s) on"

    [<Property>]
    let ``All non-remotely controlled lights that have power should be on`` () =
        genInteractions |> ensureStartsWithTimeChanged |> Arb.fromGen |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions

            let nonRemotelyControlledLightsWithPower =
                fakeHome.LightStates
                |> Seq.filter (fun (light, _) ->
                    light.ControlledWithRemote = NonRemote
                    && doesLightHavePowerAfterInteractions light interactions)
                |> Seq.toList

            nonRemotelyControlledLightsWithPower
            |> Seq.forall (snd >> _.IsOn)
            |> Prop.trivial (nonRemotelyControlledLightsWithPower.Length = 0)
            |> Prop.collect $"{nonRemotelyControlledLightsWithPower.Length} non-remotely controlled light(s) with power"

    [<Property(Arbitrary = [| typeof<ArbitraryRemotelyControlledLight> |])>]
    let ``If the remote was never used, all remote controlled lights with power should be on`` (light: Light) =
        genBiasedInteractionsExcept light _.IsRemoteInteraction
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
        genBiasedInteractions light
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
            [ genBiasedInteractions light
              |> ensureStartsWithTimeChanged
              |> ensurePartOfDayIs Night
              genTimeChangedToPartOfDay Day |> Gen.map List.singleton
              genBiasedInteractionsExcept light _.IsRemoteInteraction ]
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
            [ genBiasedInteractions light
              RemoteInteraction RemotePressedOffButton |> List.singleton |> Gen.constant
              genBiasedInteractionsExcept light (fun interaction ->
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
        genBiasedInteractions light
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
            [ genBiasedInteractions light
              RemoteInteraction RemotePressedLeftButton |> List.singleton |> Gen.constant
              genBiasedInteractionsExcept light (fun interaction ->
                  interaction.IsRemoteInteraction || interaction |> isTimeChangedToPartOfDay Day) ]
        |> ensureStartsWithTimeChanged
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions
            fakeHome.LightShouldHaveState light _.IsOff
