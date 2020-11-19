module Typecheck

open Ast
open Type

type NodeId = int

type TyCtxt =
    { types: Map<NodeId, Type> }
    static member Default = { types = Map [] }


type Tcx<'a> = Tcx of (TyCtxt -> (TyCtxt * 'a))

let runTcx (Tcx f) x = f x

type TcxBuilder() =
    member _x.Return x = Tcx(fun tcx -> (tcx, x))
    member _x.ReturnFrom(x) = x
    // f :: 'a -> Tcx<'a>
    // x :: 'a
    // (>>=) :: Tcx<'a> -> (a -> Tcx<'a>) -> Tcx<'a>
    member _x.Bind(x, f) =
        Tcx(fun tcx ->
                let (tcx', t) = runTcx x tcx
                runTcx (f t) tcx')

    member _x.Zero() = failwith ""
    member _x.Combine(p, q) = failwith ""
    member _x.Delay(f) =
        Tcx(fun src -> let (Tcx g) = f() in g src)

let tcx = TcxBuilder()
(* let execTc = runTc >>  *)

let rec typecheck expr =
    tcx {
        match expr.Kind with
        | ExprNum n -> return Int
        | ExprAdd(l, r) ->
            let! lty = typecheck l
            let! rty = typecheck r
            return Int
    }
