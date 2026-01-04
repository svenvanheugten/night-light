module NightLight.Core.Tests.NightLightTests

open FsCheck.Xunit

[<Property(Arbitrary = [| typeof<Arbitraries> |])>]
let ``Brightness should always be under 255`` (fakeHome: FakeHome) (interactions: Interaction list) =
    interactions |> Seq.iter (fun interaction -> fakeHome.Interact interaction)

    fakeHome.LightStates
    |> Seq.choose (fun (_, state) ->
        match state with
        | On brightness -> Some brightness
        | Off -> None)
    |> Seq.forall (fun brightness -> brightness < 255uy)
