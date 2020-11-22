module Resolve

open Result
open Error
open Ast
open Span
open Format
open AstVisit
open State

let resolve = state

[<RequireQualifiedAccess>]
type Res =
    | Local of NodeId
    | Err

type Resolutions =
    { Resolutions: Map<NodeId, Res> }

    member this.RecordRes idx res = { this with Resolutions = this.Resolutions.Add(idx, res) }

    static member Default = { Resolutions = Map [] }

type Scope =
    { Bindings: Map<Symbol, NodeId>
      Parent: option<Scope> }

    member this.Define symbol idx = { this with Bindings = this.Bindings.Add(symbol, idx) }

    member this.Enter =
        { this with
              Parent = Some(this)
              Bindings = Map [] }

    member this.Exit = this.Parent.Value


    member this.Lookup ident =
        match this.Bindings.TryFind ident with
        | Some idx -> Some idx
        | None ->
            optional {
                let! par = this.Parent
                return! par.Lookup ident }

    static member Default =
        { Bindings = Map []
          Parent = None }

[<NoEquality; NoComparison>]
type ResolveCtxt =
    { Resolutions: Resolutions
      Items: Map<Ident, NodeId>
      Scope: Scope }

    static member Default =
        { Resolutions = Resolutions.Default
          Scope = Scope.Default
          Items = Map [] }

type ItemCollector() =
    inherit AstVisitor<ResolveCtxt>()

    override _this.VisitItem item =
        resolve {
            // note we only consider the definition as the signature is optional
            match item.Kind with
            | ItemKind.FnDef def ->
                let! rcx = get
                do! put <| { rcx with Items = rcx.Items.Add(def.Ident, item.Id) }
            | ItemKind.Sig(_) -> return ()
        }

type Resolve<'a> = State<ResolveCtxt, 'a>

let scope =
    resolve {
        let! rcx = get
        return rcx.Scope }

let enterScope =
    resolve {
        let! scope = scope
        let! rcx = get
        do! put { rcx with Scope = scope.Enter } }

let exitScope =
    resolve {
        let! scope = scope
        let! rcx = get
        do! put { rcx with Scope = scope.Exit } }

let withScope f =
    resolve {
        do! enterScope
        let! r = f
        do! exitScope
        return r
    }

let lookup symbol =
    resolve {
        let! scope = scope
        return scope.Lookup symbol }

let recordRes idx res: Resolve<unit> =
    resolve {
        let! rcx = get
        do! put { rcx with Resolutions = rcx.Resolutions.RecordRes idx res } }

[<RequireQualifiedAccess>]
type ResErr =
    | UnboundVar of Ident
    | UnresolvedType of Ident

    interface IShow with
        member this.Show() =
            match this with
            | UnboundVar ident -> sprintf "unbound variable `%s`" (show ident)
            | UnresolvedType(_) -> failwith "Not Implemented"


let private err span kind =
    emitError span (show kind)
    Res.Err

let resolvePath path =
    resolve {
        assert (path.Segments.Length = 1)
        let ident = path.Segments.[0].Ident
        let! idx = lookup ident.Symbol
        let res =
            match idx with
            | None -> err ident.Span (ResErr.UnboundVar ident)
            | Some idx -> Res.Local idx
        return! recordRes path.Id res
    }

let declareBinding ident idx =
    resolve {
        let! rcx = get
        let! scope = scope
        let scope = scope.Define ident idx
        do! put { rcx with Scope = scope }
    }

type LateResolver() =
    inherit AstVisitor<ResolveCtxt>()

    override this.VisitFnDef def = withScope <| this.WalkFnDef def

    override this.VisitExpr expr =
        match expr.Kind with
        | ExprKind.Path path -> resolvePath path
        | _ -> this.WalkExpr expr

    override this.VisitPat pat =
        resolve {
            match pat.Kind with
            | PatKind.Bind ident -> do! declareBinding ident.Symbol pat.Id
            | _ -> ()
            do! this.WalkPat pat
        }


let runResolver = runState

let resolveAst (ast: Ast): Resolve<Resolutions> =
    resolve {
        do! ItemCollector().VisitAst ast
        do! LateResolver().VisitAst ast
        let! rcx = get
        return rcx.Resolutions
    }

let runResolveAst (ast: Ast): Resolutions = runResolver (resolveAst ast) ResolveCtxt.Default |> fst
