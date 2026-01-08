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

    let doesLightHavePowerAfter light interactions =
        interactions
        |> Seq.choose (fun interaction ->
            match interaction with
            | HumanInteraction(LightPoweredOff l) when l = light -> Some false
            | HumanInteraction(LightPoweredOn l) when l = light -> Some true
            | _ -> None)
        |> Seq.tryLast
        |> Option.defaultValue false

    [<Property(Arbitrary = [| typeof<ArbitraryLight> |])>]
    let ``All lights should be either off, white or yellow during the day`` (light: Light) =
        concatGens
            [ genInitialInteractionsAndEndWith light =<< genTimeChangedToDay
              genInteractionsExcept light isTimeChangedToNight ]
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
            [ genInitialInteractionsAndEndWith light =<< genTimeChangedToNight
              genInteractionsExcept light isTimeChangedToDay ]
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions

            fakeHome.LightShouldHaveState light (function
                | Off -> true
                | On(_, color) -> color = Red)

    [<Property(Arbitrary = [| typeof<ArbitraryNonRemotelyControlledLight> |])>]
    let ``All non-remotely controlled lights should be on *if and only if* they have power`` (light: Light) =
        genInitialInteractions light |> Arb.fromGen |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions

            doesLightHavePowerAfter light interactions = fakeHome.LightShouldHaveState light _.IsOn

    [<Property(Arbitrary = [| typeof<ArbitraryRemotelyControlledLight> |])>]
    let ``After pressing 'On' on the remote, all remotely controlled lights with power should be on, as long as the 'Off' button isn't pressed``
        (light: Light)
        =
        concatGens
            [ genInitialInteractionsAndEndWith light (HumanInteraction RemotePressedOnButton)
              genInteractionsExcept light ((=) (HumanInteraction RemotePressedOffButton)) ]
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions

            doesLightHavePowerAfter light interactions
            ==> fakeHome.LightShouldHaveState light _.IsOn

    [<Property(Arbitrary = [| typeof<ArbitraryRemotelyControlledLight> |])>]
    let ``After a new day starts, all remotely controlled lights with power should be on, as long as the 'Off' button isn't pressed``
        (light: Light)
        =
        concatGens
            [ genInitialInteractionsAndEndWith light =<< genTimeChangedToNight
              genInteractionsExcept light isTimeChangedToDay
              genTimeChangedToDay |> Gen.map List.singleton
              genInteractionsExcept light ((=) (HumanInteraction RemotePressedOffButton)) ]
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions

            doesLightHavePowerAfter light interactions
            ==> fakeHome.LightShouldHaveState light _.IsOn

    [<Property(Arbitrary = [| typeof<ArbitraryRemotelyControlledLight> |])>]
    let ``After pressing 'Off' on the remote, all remotely controlled lights should be off as long as the 'On' button isn't pressed and a new day doesn't start``
        (light: Light)
        =
        concatGens
            [ genInitialInteractionsAndEndWith light (HumanInteraction RemotePressedOffButton)
              genInteractionsExcept light (fun interaction ->
                  interaction = HumanInteraction RemotePressedOnButton
                  || interaction |> isTimeChangedToDay) ]
        |> Arb.fromGen
        |> Prop.forAll
        <| fun interactions ->
            let fakeHome = createFakeHomeWithNightLightAndInteract interactions

            fakeHome.LightShouldHaveState light _.IsOff
