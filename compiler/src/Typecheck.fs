module Typecheck

open Infer
open Resolve
open Format
open RState
open Type
open Ast
open TypeContext
open TypeLowering
open AstVisit

/// first pass walks through looking for signatures
type SigCollector() =
    inherit AstVisitor<TyCtxt, TypeError>()

    override _.VisitItem item =
        tcx {
            match item.Kind with
            | ItemKind.Sig fnsig ->
                let! ty = astTyToTy fnsig.Type
                do! recordSig item.Id ty
            | ItemKind.FnDef _ -> return ()
        }

/// second pass walks through looking for function definitions
type FnDefCollector() =
    inherit AstVisitor<TyCtxt, TypeError>()

    override _.VisitItem item =
        tcx {
            match item.Kind with
            | ItemKind.FnDef _ ->
                let! tyvar = newTyvar
                do! recordSig item.Id tyvar
            | ItemKind.Sig _ -> return ()
        }

let checkLit (lit: Lit): Tcx<Ty> =
    tcx {
        match lit.Kind with
        | LitKind.LitBool _ -> return mkTy TyKind.Bool
        | LitKind.LitInt _ -> return mkTy TyKind.Int
    }

let rec checkPat (pat: Pat): Tcx<Ty> =
    tcx {
        match pat.Kind with
        | PatKind.Tuple(pats) ->
            let! tys = mapM checkPat pats
            return mkTy (TyKind.Tuple tys)
        | PatKind.Bind _ ->
            let! tyvar = newTyvar
            return! recordTy pat.Id tyvar
    }


let checkExprPath expr (path: Path): Tcx<Ty> =
    tcx {
        let! res = getRes path
        match res with
        | Res.Local nodeId -> return! nodeTy nodeId
        | Res.Err -> return! mkTyErr
    }

let rec checkExpr (expr: Expr): Tcx<Ty> =
    tcx {
        let! ty = match expr.Kind with
                  | ExprKind.Path path -> checkExprPath expr path
                  | ExprKind.Tuple(_) -> mkTyErr
                  | ExprKind.App(f, arg) -> checkExprApp expr f arg
                  | ExprKind.Lit(lit) -> checkLit lit
                  | ExprKind.Bin(op, l, r) -> mkTyErr
                  | ExprKind.Unary(_, _) -> mkTyErr
        return! recordTy expr.Id ty
    }

and checkExprApp expr f arg: Tcx<Ty> =
    tcx {
        let! fty = checkExpr f
        let! arg = checkExpr arg
        let! rty = newTyvar
        let! _ = unify expr fty <| mkTy (TyKind.Fn(arg, rty))
        return rty }

/// converts uncurried type into curried type
let rec curriedTy (paramTys: list<Ty>) (retTy: Ty): Ty =
    match paramTys with
    | [] -> retTy
    | t :: ts -> mkTy <| TyKind.Fn(t, curriedTy ts retTy)

type Typechecker() =
    inherit AstVisitor<TyCtxt, TypeError>()
    override _.VisitItem item =
        tcx {
            match item.Kind with
            | ItemKind.FnDef def ->
                let! paramTys = mapM checkPat def.Params
                let! bodyTy = checkExpr def.Body
                let fnTy = curriedTy paramTys bodyTy
                printfn "%s :: %s" (show def.Ident) (show fnTy)
                return ()
            | ItemKind.Sig _ -> return ()
        }

let collectFnSigs ast = tcx { do! SigCollector().VisitAst ast }
let collectFnDefs ast = tcx { do! FnDefCollector().VisitAst ast }
let check ast = tcx { do! Typechecker().VisitAst ast }

let generateOutputs: Tcx<TypecheckOutputs> = tcx { return failwith "" }

let typecheck ast =
    tcx {
        do! collectFnSigs ast
        do! collectFnDefs ast
        do! check ast
    }

let runTypecheck resolutions ast = evalTcx (typecheck ast) (TyCtxt.New resolutions)
