module Infer

open Type
open State
open TypeContext
open Unify

type Tyvid = int

type InferCtxt =
    { Tcx: TyCtxt
      Tyvars: UnificationTable<Ty> }

    static member New tcx =
        { Tyvars = UnificationTable()
          Tcx = tcx }


let withInferCtxt tcx f = InferCtxt.New tcx |> f

type Infcx<'a> = State<'a, InferCtxt>

let infcx = state

let unify s t = infcx { failwith "" }
