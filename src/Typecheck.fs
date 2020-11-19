module Typecheck

open Ast
open Infer
open Type
open TypeContext


let rec typecheck expr =
    tcx {
        let! ty = match expr.Kind with
                  | ExprNum _ -> tcx { return Int }
                  | ExprAdd(l, r) -> typecheckAdd expr l r
        do! recordTy expr.Id ty
        return ty
    }

and typecheckAdd _expr l r =
    tcx {
        let! _lty = typecheck l
        let! _rty = typecheck r
        return Int }

let runTypecheck expr = execTcx (typecheck expr) TyCtxt.Default
