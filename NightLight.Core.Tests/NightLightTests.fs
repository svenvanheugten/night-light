namespace NightLight.Core.Tests

open NightLight.Core.Core
open NightLight.Core.Tests.InteractionListGenerators
open NightLight.Core.Tests.InteractionListHelpers
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

    [<Property(Arbitrary = [| typeof<ArbitraryInteractions> |])>]
    let ``All lights should either be off or have the right color`` (interactions: Interaction list) =
        let fakeHome = createFakeHomeWithNightLightAndInteract interactions
        let partOfDay = getPartOfDayAfterInteractions interactions

        fakeHome.LightStates
        |> Seq.forall (function
            | _, Off -> true
            | _, On(_, color) ->
                match partOfDay with
                | Day -> color = White || color = Yellow
                | Night -> color = Red)
        |> Prop.collect partOfDay
        |> Prop.collect $"{fakeHome.LightsThatAreOn.Length} light(s) on"
        |> Prop.label fakeHome.Label
        |> Prop.trivial (fakeHome.LightsThatAreOn.Length = 0)

    [<Property(Arbitrary = [| typeof<ArbitraryInteractions> |])>]
    let ``All lights should either be off or have a brightness that fits its color`` (interactions: Interaction list) =
        let fakeHome = createFakeHomeWithNightLightAndInteract interactions
        let time = getTimeAfterInteractions interactions |> _.TimeOfDay

        let alarm =
            hasNewDayStartedSince interactions (tryGetLastRemoteInteraction interactions)
            && startOfDay <= time
            && time <= endOfAlarm

        let scaledForAlarm light brightness =
            if light.ControlledWithRemote <> NonRemote && alarm then
                float brightness * ((time - startOfDay) / (endOfAlarm - startOfDay)) |> byte
            else
                brightness

        fakeHome.LightStates
        |> Seq.forall (fun (light, state) ->
            let maybeExpectedBrightness =
                match light, state with
                | _, Off -> None
                | { Bulb = IkeaBulb }, On(_, White) -> Some 254uy
                | { Bulb = IkeaBulb }, On(_, Yellow) -> Some 210uy
                | { Bulb = IkeaBulb }, On(_, Red) -> Some 254uy
                | { Bulb = PaulmannBulb }, On(_, White) -> Some 35uy
                | { Bulb = PaulmannBulb }, On(_, Yellow) -> Some 35uy
                | { Bulb = PaulmannBulb }, On(_, Red) -> Some 80uy
                |> Option.map (scaledForAlarm light)

            let maybeActualBrightness =
                match state with
                | Off -> None
                | On(brightness, _) -> Some brightness

            maybeExpectedBrightness = maybeActualBrightness)
        |> Prop.collect $"{fakeHome.LightsThatAreOn.Length} light(s) on"
        |> Prop.classify alarm "alarm"
        |> Prop.label fakeHome.Label
        |> Prop.trivial (fakeHome.LightsThatAreOn.Length = 0)

    [<Property(Arbitrary = [| typeof<ArbitraryInteractions> |])>]
    let ``All non-remotely controlled lights with power should be on`` (interactions: Interaction list) =
        let fakeHome = createFakeHomeWithNightLightAndInteract interactions

        let nonRemotelyControlledLightsWithPower =
            fakeHome.NonRemotelyControlledLightStates
            |> Seq.filter (fun (light, _) -> doesLightHavePowerAfterInteractions light interactions)
            |> Seq.toList

        nonRemotelyControlledLightsWithPower
        |> Seq.forall (snd >> _.IsOn)
        |> Prop.collect $"{nonRemotelyControlledLightsWithPower.Length} non-remotely controlled light(s) with power"
        |> Prop.label fakeHome.Label
        |> Prop.trivial (nonRemotelyControlledLightsWithPower.Length = 0)

    [<Property(Arbitrary = [| typeof<ArbitraryInteractions> |])>]
    let ``All remotely-controlled lights with power should have the correct state`` (interactions: Interaction list) =
        let fakeHome = createFakeHomeWithNightLightAndInteract interactions

        let remotelyControlledLightsWithPower =
            fakeHome.RemotelyControlledLightStates
            |> Seq.filter (fun (light, _) -> doesLightHavePowerAfterInteractions light interactions)
            |> Seq.toList

        let allOn (ls: (Light * LightState) seq) = ls |> Seq.forall (snd >> _.IsOn)
        let allOff (ls: (Light * LightState) seq) = ls |> Seq.forall (snd >> _.IsOff)

        let controlledBy remote ls =
            ls |> Seq.filter (fst >> _.ControlledWithRemote >> (=) remote)

        let maybeLastRemoteInteraction = tryGetLastRemoteInteraction interactions

        let hasNewDayStartedSinceThen =
            hasNewDayStartedSince interactions maybeLastRemoteInteraction

        if hasNewDayStartedSinceThen then
            remotelyControlledLightsWithPower |> allOn
        else
            match maybeLastRemoteInteraction with
            | Some(_, RemotePressedOnButton) -> remotelyControlledLightsWithPower |> allOn
            | Some(_, RemotePressedOffButton) -> remotelyControlledLightsWithPower |> allOff
            | Some(_, RemotePressedLeftButton) ->
                remotelyControlledLightsWithPower |> controlledBy RemoteLeft |> allOn
                && remotelyControlledLightsWithPower |> controlledBy RemoteRight |> allOff
            | None -> remotelyControlledLightsWithPower |> allOn
        |> Prop.collect $"last remote interaction is {maybeLastRemoteInteraction |> Option.map snd}"
        |> Prop.collect $"{remotelyControlledLightsWithPower.Length} remotely controlled light(s) with power"
        |> Prop.classify hasNewDayStartedSinceThen "new day has started since then"
        |> Prop.label fakeHome.Label
        |> Prop.trivial (remotelyControlledLightsWithPower.Length = 0)
