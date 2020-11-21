module TypeContext

open Ast
open Type

type TyCtxt =
    { NodeTypes: Map<NodeId, Ty> }
    static member Default = { NodeTypes = Map [] }


type Tcx<'a> = Tcx of (TyCtxt -> (TyCtxt * 'a))

let runTcx (Tcx f) x = f x

let execTcx tcx x = runTcx tcx x |> fst

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

let private get = Tcx(fun tcx -> (tcx, tcx))
let private put tcx = Tcx(fun _ -> (tcx, ()))

let recordTy nodeId ty =
    tcx {
        let! tcx = get
        let fuck = tcx.NodeTypes.Add(nodeId, ty)
        do! put { tcx with NodeTypes = fuck }
    }
