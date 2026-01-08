module NightLight.Core.Tests.GenHelpers

open FsCheck
open FsCheck.FSharp

let concatGens (gens: Gen<'a list> list) : Gen<'a list> =
    match gens with
    | [] -> Gen.constant []
    | first :: rest -> rest |> List.fold (fun accGen g -> Gen.map2 (@) accGen g) first

let (=<<) m f = Gen.bind m f
