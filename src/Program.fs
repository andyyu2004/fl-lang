module Main

open System.IO
open Lex
open Format
open Parse


[<EntryPoint>]
let main argv =
    let src = File.ReadAllText(argv.[0]) |> Seq.toList
    let tokens = lexProgram src
    let ast = parseProgram tokens
    match ast with
    | Ok ast -> printfn "%s" (show ast)
    | Error _err -> printfn "parse error"
    0
