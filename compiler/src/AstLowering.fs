/// lowering `Ast` representation to `TAst`
module AstLowering

open Resolve
open TypeContext
open RState
open TypedAst
open Ast
open Error
open Infer

let rec lowerPat (pat: Pat): Tcx<TPat> =
    tcx {
        let! kind = match pat.Kind with
                    | PatKind.Tuple(pats) -> TPatKind.Tuple <+> mapM lowerPat pats
                    | PatKind.Bind ident -> tcx { return TPatKind.Bind ident }
        let! patTy = nodeTy pat.Id
        printfn "ty: %O" patTy
        let! ty = fullyResolveTy patTy
        printfn "fully resolved ty: %O" ty
        return { Id = pat.Id
                 Span = pat.Span
                 Ty = ty
                 Kind = kind }
    }

let rec lowerExprPath (path: Path): Tcx<TExprKind> =
    tcx {
        let! res = getRes path
        match res with
        | Res.Local idx -> return TExprKind.Var idx
        | Res.Err -> return failwith "resolution error"
    }

let rec lowerExprLit (lit: Lit): Tcx<TExprKind> = tcx { return TExprKind.Lit lit }


let rec lowerExpr (expr: Expr): Tcx<TExpr> =
    tcx {
        let! kind = match expr.Kind with
                    | ExprKind.Path path -> lowerExprPath path
                    | ExprKind.Tuple(_) -> failwith "Not Implemented"
                    | ExprKind.App(f, arg) -> lowerExprApp f arg
                    | ExprKind.Lit lit -> lowerExprLit lit
                    | ExprKind.Unary(_, _) -> failwith "Not Implemented"
                    | ExprKind.Bin(_, _, _) -> failwith "Not Implemented"
        let! exprTy = nodeTy expr.Id
        let! ty = fullyResolveTy exprTy
        return { Id = expr.Id
                 Span = expr.Span
                 Ty = ty
                 Kind = kind }
    }

and lowerExprApp f arg: Tcx<TExprKind> =
    tcx {
        let! f = lowerExpr f
        let! arg = lowerExpr arg
        return TExprKind.App(f, arg) }

let lowerFnDef (def: FnDef): Tcx<TItemKind> =
    tcx {
        let! pats = mapM lowerPat def.Params
        let! body = lowerExpr def.Body
        return TItemKind.FnDef
                   { Ident = def.Ident
                     Params = pats
                     Body = body }
    }

// not all ast itemkinds have a corresponding variant in the typed ast
// so this function returns an option
let lowerItem (item: Item): Tcx<option<TItem>> =
    tcx {
        let! kind = match item.Kind with
                    | ItemKind.FnDef def -> Some <+> lowerFnDef def
                    | ItemKind.Sig _ -> tcx { return None }
        return optional {
                   let! kind = kind
                   return { Id = item.Id
                            Span = item.Span
                            Kind = kind }
               }
    }

let lowerAst (ast: Ast): Tcx<TAst> =
    tcx {
        let! items = mapM lowerItem ast.Items
        let items =
            items
            |> List.filter Option.isSome
            |> List.map Option.get

        return { Items = items }
    }
