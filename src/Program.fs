module Main

open System.IO
open Lex
open Parse


[<EntryPoint>]
let main argv =
    let src = File.ReadAllText(argv.[0]) |> Seq.toList
    let tokens = lexProgram src
    printfn "%A" tokens
    (* let ast = parseProgram tokens *)
    0
