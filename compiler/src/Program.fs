module Compiler

open System.IO
open Lex
open Format
open Parse

let runLexPhase path: list<Token> =
    let src = File.ReadAllText path |> Seq.toList
    lexProgram src

let runParsePhase path =
    let tokens = runLexPhase path
    parseProgram tokens

let runCompiler path =
    let ast = runParsePhase path
    match ast with
    | Ok ast -> printfn "%s" (show ast)
    | Error _err -> printfn "parse error"

[<EntryPoint>]
let main argv =
    runCompiler argv.[0]
    0
