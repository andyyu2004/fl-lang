module Infer

open Type

type Tyvid = int

type InferCtxt =
    { tyvars: Map<Tyvid, Type> }
    static member Default = { tyvars = Map [] }


type Infcx<'a> = Infcx of (InferCtxt -> (InferCtxt * 'a))

let runInfcx (Infcx f) x = f x

type TcxBuilder() =
    member _x.Return x = Infcx(fun tcx -> (tcx, x))
    member _x.ReturnFrom(x) = x
    // f :: 'a -> Tcx<'a>
    // x :: 'a
    // (>>=) :: Tcx<'a> -> (a -> Tcx<'a>) -> Tcx<'a>
    member _x.Bind(x, f) =
        Infcx(fun infcx ->
            let (infcx', t) = runInfcx x infcx
            runInfcx (f t) infcx')

    member _x.Zero() = failwith ""
    member _x.Combine(p, q) = failwith ""
    member _x.Delay(f) =
        Infcx(fun src ->
            let (Infcx g) = f() in g src)

let infcx = TcxBuilder()
