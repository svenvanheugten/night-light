namespace NightLight.Core.Tests

open System
open FsCheck
open FsCheck.FSharp
open NightLight.Core.Models

type Arbitraries =
    static member Interactions() : Arbitrary<Interaction list> =
        gen {
            let genTimeChangedInteraction =
                gen {
                    let! time = ArbMap.defaults |> ArbMap.generate<DateTime>
                    return Interaction.TimeChanged time
                }

            let genHumanInteraction =
                gen {
                    let! light = Gen.elements lights
                    let! humanInteraction = Gen.elements [ LightTurnedOn light; LightTurnedOff light ]
                    return Interaction.HumanInteraction humanInteraction
                }

            let! initialTimeChangedInteraction = genTimeChangedInteraction
            let! remainingInteractions = Gen.oneof [ genTimeChangedInteraction; genHumanInteraction ] |> Gen.listOf

            return initialTimeChangedInteraction :: remainingInteractions
        }
        |> Arb.fromGen
