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

type IRelate =
    abstract Relate: IRelation -> IRelate -> IRelate -> Tcx<IRelate>


type TypeFolder = Ty -> Tcx<Ty>

let private foldTyInner (ty: Ty) (f: TypeFolder): Tcx<Ty> =
    tcx {
        match ty.Kind with
        | _ -> return failwith ""
    }
