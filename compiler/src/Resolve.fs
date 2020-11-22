module Resolve

open Ast
open State

type Res =
    | Local of NodeId
    | Err

type Resolutions =
    { Resolutions: Map<NodeId, Res> }

    static member Default = { Resolutions = Map [] }

[<NoEquality; NoComparison>]
type ResolveCtxt =
    { Resolutions: Resolutions }
    static member Default = { Resolutions = Resolutions.Default }

type Resolve<'a> = State<ResolveCtxt, 'a>

let resolve = state

let runResolver = runState


let resolveItem (item: Item): Resolve<unit> =
    match item.Kind with
    | ItemKind.Fn fn -> failwith "todo"

let resolveAst (ast: Ast): Resolve<Resolutions> =
    resolve {
        let! _ = mapM resolveItem ast.Items
        let! rcx = get
        return rcx.Resolutions }

let runResolveAst (ast: Ast): Resolutions = runResolver (resolveAst ast) ResolveCtxt.Default |> fst
