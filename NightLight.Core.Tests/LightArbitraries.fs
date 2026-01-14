namespace NightLight.Core.Tests

open NightLight.Core.Models
open FsCheck.FSharp

type ArbitraryLight =
    static member Light() = lights |> Gen.elements |> Arb.fromGen

type ArbitraryNonRemotelyControlledLight =
    static member Light() =
        lights
        |> Seq.filter _.ControlledWithRemote.IsNonRemote
        |> Gen.elements
        |> Arb.fromGen

type ArbitraryLeftRemotelyControlledLight =
    static member Light() =
        lights
        |> Seq.filter _.ControlledWithRemote.IsRemoteLeft
        |> Gen.elements
        |> Arb.fromGen

type ArbitraryRightRemotelyControlledLight =
    static member Light() =
        lights
        |> Seq.filter _.ControlledWithRemote.IsRemoteRight
        |> Gen.elements
        |> Arb.fromGen

type ArbitraryRemotelyControlledLight =
    static member Light() =
        lights
        |> Seq.filter (fun light ->
            match light.ControlledWithRemote with
            | RemoteLeft -> true
            | RemoteRight -> true
            | NonRemote -> false)
        |> Gen.elements
        |> Arb.fromGen
