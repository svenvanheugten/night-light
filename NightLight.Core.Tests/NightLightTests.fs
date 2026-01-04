namespace NightLight.Core.Tests

open System
open FsCheck.Xunit
open FsCheck.FSharp

[<Properties(Arbitrary = [| typeof<Arbitraries> |])>]
type NightLightTests() =
    [<Property>]
    let ``Brightness should always be under 255`` (now: DateTime) (interactions: Interaction list) =
        let fakeHome = FakeHome now
        fakeHome.Interact interactions
        fakeHome.ForAllLightsThatAreOn(fun (_, brightness, _) -> brightness < 255uy)

    [<Property>]
    let ``Lights should be red during the night`` (now: DateTime) (interactions: Interaction list) =
        let fakeHome = FakeHome now
        fakeHome.Interact interactions

        fakeHome.IsNight()
        ==> fakeHome.ForAllLightsThatAreOn(fun (_, _, color) -> color = Red)

    [<Property>]
    let ``Lights should be white or yellow during the day`` (now: DateTime) (interactions: Interaction list) =
        let fakeHome = FakeHome now
        fakeHome.Interact interactions

        fakeHome.IsDay()
        ==> fakeHome.ForAllLightsThatAreOn(fun (_, _, color) -> color = White || color = Yellow)
