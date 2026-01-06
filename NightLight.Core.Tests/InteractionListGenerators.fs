module NightLight.Core.Tests.InteractionListGenerators

open FsCheck.FSharp
open NightLight.Core.Models
open NightLight.Core.Tests.GenHelpers
open NightLight.Core.Tests.TimeChangedGenerators

let private genHumanInteraction =
    let genLightInteraction =
        Gen.elements lights
        |> Gen.bind (fun light -> Gen.elements [ LightPoweredOn light; LightPoweredOff light ])

    let genRemoteInteraction =
        Gen.elements [ RemotePressedOnButton; RemotePressedOffButton ]

    Gen.oneof [ genLightInteraction; genRemoteInteraction ]
    |> Gen.map Interaction.HumanInteraction

let private genInteraction = Gen.oneof [ genTimeChanged; genHumanInteraction ]

let private genInteractionsListThatStartsWithTimeChanged =
    [ genTimeChanged |> Gen.map List.singleton; Gen.listOf genInteraction ]
    |> concatGens

let genInteractionListThatStartsWithTimeChangedAndEndsWith (endsWith: Interaction) =
    let genNonTrivialList =
        genInteractionsListThatStartsWithTimeChanged
        |> Gen.map (fun lst -> lst @ [ endsWith ])

    match endsWith with
    | Interaction.TimeChanged _ ->
        let genTrivialList = Gen.constant <| List.singleton endsWith
        Gen.frequency [ 1, genTrivialList; 9, genNonTrivialList ]
    | _ -> genNonTrivialList

let genInteractionListExcept disqualifier =
    genInteraction |> Gen.filter (not << disqualifier) |> Gen.listOf
