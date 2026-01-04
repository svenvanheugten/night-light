namespace NightLight.Core.Tests

open System
open FsCheck.FSharp
open NightLight.Core.Models

type Arbitraries =
    static member Interaction() =
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

        Gen.oneof [ genTimeChangedInteraction; genHumanInteraction ] |> Arb.fromGen
