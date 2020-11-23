module Infer

open Span
open Type
open RState
open Unify
open TypeContext

let newTyvar: Tcx<Ty> =
    tcx {
        let! tcx = get
        return Ty.TyVar(tcx.Tyvars.NewKey TyVarValue.Unconstrained) }

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
        return Ty.TyVar x
    }



[<AbstractClass>]
type IRelation() =
    abstract RelateTys: Ty -> Ty -> Tcx<Ty>

    member this.RelateTysInner s t =
        assert (s <> t)
        tcx {
            match (s, t) with
            | (Ty.Tuple(xs), Ty.Tuple(ys)) ->
                let! ts = List.zip xs ys
                          |> List.map (fun (x, y) -> this.RelateTys x y)
                          |> sequence
                return Ty.Tuple(ts)
            | (Ty.Fn(fl, rl), Ty.Fn(fr, rr)) ->
                let! f = this.RelateTys fl fr
                let! r = this.RelateTys rl rr
                return Ty.Fn(f, r)
            | _ -> return Ty.Err
            return s
        }

type IRelate =
    abstract Relate: IRelation -> IRelate -> IRelate -> Tcx<IRelate>

type Equate() =
    inherit IRelation()
    override this.RelateTys s t =
        tcx {
            if s = t then
                return s
            else
                match (s, t) with
                | (Ty.TyVar x, Ty.TyVar y) -> return! unifyTyVars x y
                | (Ty.TyVar x, _) -> return! instantiate x t
                | (_, Ty.TyVar y) -> return! instantiate y s
                | _ -> return! this.RelateTysInner s t
        }



let unify (span: ISpanned) = Equate().RelateTys
