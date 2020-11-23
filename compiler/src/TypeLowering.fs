module TypeLowering

open RState
open Ast
open Type
open TypeContext


(* type ITyConv = *)
(*     abstract InferTy: unit -> InferTy *)
(*     abstract AllowInfer: bool *)
(* member this.AstTyToTy(astTy: AstTy): Ty = *)
(*     match astTy.Kind with *)
(*     | AstTyKind.Bool -> Ty.Bool *)
(*     | AstTyKind.Int -> Ty.Int *)
(*     | AstTyKind.Tuple(tys) -> *)
(*         let tys = List.map this.AstTyToTy tys *)
(*         Ty.Tuple tys *)
(*     | AstTyKind.Fn(param, ret) -> *)
(*         let paramTy = this.AstTyToTy param *)
(*         let retTy = this.AstTyToTy ret *)
(*         Ty.Fn(paramTy, retTy) *)
(*     | AstTyKind.Path(_) -> failwith "Not Implemented" *)

let rec astTyToTy (astTy: AstTy): Tcx<Ty> =
    tcx {
        match astTy.Kind with
        | AstTyKind.Bool -> return Ty.Bool
        | AstTyKind.Int -> return Ty.Int
        | AstTyKind.Tuple(tys) ->
            let! tys = mapM astTyToTy tys
            return Ty.Tuple tys
        | AstTyKind.Fn(param, ret) ->
            let! paramTy = astTyToTy param
            let! retTy = astTyToTy ret
            return Ty.Fn(paramTy, retTy)
        | AstTyKind.Path(_) -> return failwith "Not Implemented"
    }
