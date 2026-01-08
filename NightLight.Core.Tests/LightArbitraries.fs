namespace NightLight.Core.Tests

open NightLight.Core.Models
open FsCheck.FSharp

type ArbitraryLight =
    static member Light() = lights |> Gen.elements |> Arb.fromGen

type ArbitraryNonRemotelyControlledLight =
    static member Light() =
        lights
        |> Seq.filter (not << _.ControlledWithRemote)
        |> Gen.elements
        |> Arb.fromGen

type ArbitraryRemotelyControlledLight =
    static member Light() =
        lights |> Seq.filter _.ControlledWithRemote |> Gen.elements |> Arb.fromGen
