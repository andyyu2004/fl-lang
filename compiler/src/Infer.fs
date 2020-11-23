module Infer

open Span
open TypeTraversal
open TypeContext
open Type
open RState
open Unify

let newTyvar: Tcx<Ty> =
    tcx {
        let! tcx = get
        return mkTy <| TyKind.TyVar(tcx.Tyvars.NewKey TyVarValue.Unconstrained) }

let instantiate tyvid ty: Tcx<Ty> =
    tcx {
        let! tcx = get
        tcx.Tyvars.UnifyKV tyvid (TyVarValue.Known ty)
        return ty
    }

let unifyTyVars x y: Tcx<Ty> =
    tcx {
        let! tcx = get
        tcx.Tyvars.Union x y
        return mkTy (TyKind.TyVar x)
    }



let rec equateRelation s t =
    tcx {
        if s = t then
            return s
        else
            match (s.Kind, t.Kind) with
            | (TyKind.TyVar x, TyKind.TyVar y) -> return! unifyTyVars x y
            | (TyKind.TyVar x, _) -> return! instantiate x t
            | (_, TyKind.TyVar y) -> return! instantiate y s
            | _ -> return! relateTys equateRelation s t
    }

// TODO check for errors? with catch?
let unify (span: ISpanned) = equateRelation

/// partially resolves a type variable
let rec partiallyResolveTy ty =
    tcx {
        match ty.Kind with
        | TyKind.TyVar var ->
            match! probeTyvar var with
            | TyVarValue.Known ty' -> return! partiallyResolveTy ty'
            | TyVarValue.Unconstrained -> return ty
        | _ -> return ty
    }

let fullyResolveTy = foldTy partiallyResolveTy
