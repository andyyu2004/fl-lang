module Main

open Parse
open System.IO




[<EntryPoint>]
let main argv =
    let src = File.ReadAllText(argv.[0])
    Parse.parseExpr src |> ignore

    0
