module TypeTraversal

open Type
open TypeContext
open RState


[<AbstractClass>]
type IRelation() =
    abstract RelateTys: Ty -> Ty -> Tcx<Ty>

    member this.RelateTysInner s t =
        assert (s <> t)
        tcx {
            match (s.Kind, t.Kind) with
            | (TyKind.Tuple(xs), TyKind.Tuple(ys)) ->
                let! ts = List.zip xs ys
                          |> List.map (fun (x, y) -> this.RelateTys x y)
                          |> sequence
                return TyKind.Tuple(ts)
            | (TyKind.Fn(fl, rl), TyKind.Fn(fr, rr)) ->
                let! f = this.RelateTys fl fr
                let! r = this.RelateTys rl rr
                return TyKind.Fn(f, r)
            | _ -> return TyKind.Err
            return s
        }


type TypeFolder = Ty -> Tcx<Ty>

type TypeRelation = Ty -> Ty -> Tcx<Ty>


let rec relateTys (r: TypeRelation) (s: Ty) (t: Ty) =
    assert (s <> t)
    tcx {
        match (s.Kind, t.Kind) with
        | (TyKind.Tuple(xs), TyKind.Tuple(ys)) ->
            let! ts = List.zip xs ys
                      |> List.map (fun (x, y) -> relateTys r x y)
                      |> sequence
            return TyKind.Tuple(ts)
        | (TyKind.Fn(fl, rl), TyKind.Fn(fr, rr)) ->
            let! f = relateTys r fl fr
            let! r = relateTys r rl rr
            return TyKind.Fn(f, r)
        | _ -> return TyKind.Err
        return s
    }

let rec foldTy (f: TypeFolder) (ty: Ty): Tcx<Ty> =
    tcx {
        let! ty = f ty
        return! foldTyInner f ty }


and private foldTyInner f ty: Tcx<Ty> =
    tcx {
        match ty.Kind with
        | TyKind.Tuple(xs) ->
            let! tys = mapM (foldTy f) xs
            return mkTy <| TyKind.Tuple tys
        | TyKind.Fn(t, arg) ->
            let! t = foldTy f t
            let! arg = foldTy f arg
            return mkTy <| TyKind.Fn(t, arg)
        | TyKind.TyVar(_)
        | TyKind.Int
        | TyKind.Bool
        | TyKind.Err -> return ty
    }
