module TypeLowering

open Ast
open Type

let rec astTyToTy (astTy: AstTy): Ty =
    match astTy.Kind with
    | AstTyKind.Bool -> Ty.Bool
    | AstTyKind.Int -> Ty.Int
    | AstTyKind.Tuple(tys) -> Ty.Tuple <| List.map astTyToTy tys
    | AstTyKind.Fn(param, ret) -> Ty.Fn(astTyToTy param, astTyToTy ret)
    | AstTyKind.Path(_) -> failwith "Not Implemented"
