namespace NightLight.Core.Tests

open FsCheck.Xunit

[<Properties(Arbitrary = [| typeof<Arbitraries> |])>]
type NightLightTests() =
    [<Property>]
    let ``Brightness should always be under 255`` (fakeHome: FakeHome) (interactions: Interaction list) =
        interactions |> Seq.iter (fun interaction -> fakeHome.Interact interaction)

        fakeHome.LightStates
        |> Seq.choose (fun (_, state) ->
            match state with
            | On(brightness, _) -> Some brightness
            | Off -> None)
        |> Seq.forall (fun brightness -> brightness < 255uy)
