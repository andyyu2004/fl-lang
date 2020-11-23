module Compiler

open System.IO
open Lex
open TypeContext
open AstLowering
open Typecheck
open Format
open Parse
open Resolve

type ErrorEmitted =
    | ErrorEmitted
    interface IShow with
        member _.Show() = "error emitted"


type DriverBuilder() =
    member _x.Return t = Ok t
    member _x.ReturnFrom(x) = x

    member _x.Bind(t, f) =
        match t with
        | Error err ->
            eprintfn "%s" (show err)
            Error ErrorEmitted
        | Ok x ->
            match f x with
            | Ok x -> Ok x
            | Error err ->
                eprintfn "%s" (show err)
                Error ErrorEmitted

    member _x.Zero() = Ok()

    member x.Combine(p, q) = x.Bind(p, (fun _ -> q))

let driver = DriverBuilder()


let runLexPhase path: list<Token> =
    let src = File.ReadAllText path |> Seq.toList
    lexProgram src

let runParsePhase path =
    let tokens = runLexPhase path
    parseProgram tokens

let runResolutionPhase path =
    driver {
        let! ast = runParsePhase path
        let! resolutions = runResolveAst ast
        return (ast, resolutions) }

let runTypecheckPhase path =
    driver {
        let! (ast, resolutions) = runResolutionPhase path
        printfn "%O" ast
        printfn "%A" resolutions
        let tyctx = runTypecheck resolutions ast
        printfn "%O" tyctx.Tyvars
        return (ast, tyctx)
    }

let runAstLoweringPhase path =
    driver {
        let! (ast, tyctx) = runTypecheckPhase path
        let! typedAst = execTcx (lowerAst ast) tyctx
        printfn "%O" typedAst }


(* printfn "%A" tcx *)

let runCompiler path = runAstLoweringPhase path

[<EntryPoint>]
let main argv =
    runCompiler argv.[0] |> ignore
    0
