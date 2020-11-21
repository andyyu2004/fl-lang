module Typecheck



let rec typecheck (expr: Ast.Expr) = expr
(*     tcx { *)
(*         let! ty = match expr.Kind with *)
(*                   | ExprBin(op, l, r) -> typecheckAdd expr l r *)
(*                   | ExprUnary(op, operand) -> failwith "" *)
(*                   | ExprLit _lit -> failwith "" *)
(*         do! recordTy expr.Id ty *)
(*         return ty *)
(*     } *)
(* and typecheckAdd _expr l r = *)
(*     tcx { *)
(*         let! _lty = typecheck l *)
(*         let! _rty = typecheck r *)
(*         return TyInt } *)
(* let runTypecheck expr = execTcx (typecheck expr) TyCtxt.Default *)
