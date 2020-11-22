module Typecheck

open TypeContext
open Ast

let checkItem item = failwith ""

let collect ast = tcx { failwith "" }

let typecheck ast = tcx { do! collect ast }

let runTypecheck resolutions ast = execTcx (typecheck ast) (TyCtxt.New resolutions)
