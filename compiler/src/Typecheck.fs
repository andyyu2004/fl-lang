module Typecheck

open TypeContext
open Ast


let checkItem item = failwith ""
let typecheck (ast: Ast) = failwith ""
let runTypecheck ast = execTcx (typecheck ast) TyCtxt.Default
