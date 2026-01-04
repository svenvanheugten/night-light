module NightLight.Core.Tests.NightLightTests

open FsCheck.Xunit

let private assertIsOk (result: Result<unit, 'a>) : unit =
    match result with
    | Ok() -> ()
    | Error error -> failwith $"Expected Ok, got Error {error}"

[<Property(Arbitrary = [| typeof<Arbitraries> |])>]
let ``Brightness should always be under 255`` (fakeHome: FakeHome) (interactions: Interaction list) =
    interactions
    |> Seq.iter (fun interaction -> fakeHome.Interact interaction |> assertIsOk)

    fakeHome.LightStates
    |> Seq.choose (fun (_, state) ->
        match state with
        | On brightness -> Some brightness
        | Off -> None)
    |> Seq.forall (fun brightness -> brightness < 255uy)
