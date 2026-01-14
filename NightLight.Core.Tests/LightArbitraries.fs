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
        remoteControlledLights
        |> Seq.filter _.ControlledWithRemote.IsRemoteLeft
        |> Gen.elements
        |> Arb.fromGen

type ArbitraryRightRemotelyControlledLight =
    static member Light() =
        remoteControlledLights
        |> Seq.filter _.ControlledWithRemote.IsRemoteRight
        |> Gen.elements
        |> Arb.fromGen

type ArbitraryRemotelyControlledLight =
    static member Light() =
        remoteControlledLights |> Gen.elements |> Arb.fromGen
