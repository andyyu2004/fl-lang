module Main

open System.IO
open Lex




[<EntryPoint>]
let main argv =
    let src = File.ReadAllText(argv.[0]) |> Seq.toList
    (* Parse.parseProgram src |> ignore *)
    let tokens = lexProgram src
    printfn "%A" tokens
    0
