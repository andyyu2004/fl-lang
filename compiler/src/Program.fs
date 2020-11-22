module Compiler

open System.IO
open Lex
open Format
open Parse
open Resolve

let runLexPhase path: list<Token> =
    let src = File.ReadAllText path |> Seq.toList
    lexProgram src

let runParsePhase path =
    let tokens = runLexPhase path
    parseProgram tokens

let runResolutionPhase path =
    let ast = runParsePhase path
    match ast with
    | Error err -> failwith <| sprintf "%s" (show err)
    | Ok ast -> (ast, runResolveAst ast)


let runCompiler path =
    let (ast, resolutions) = runResolutionPhase path
    printfn "%s" (show ast)
    printfn "%A" resolutions

[<EntryPoint>]
let main argv =
    runCompiler argv.[0]
    0
