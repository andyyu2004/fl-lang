module Ast

open Lex

type NodeId =
    private { inner: int }

(* ast representation of types *)

type Type =
    | AstTyInt
    | AstTyBool
    | AstTyFn of Type * Type

(* type signature *)
type Sig = Type

type FunctionItem =
    { Ident: Ident
      Sig: Sig }


type ItemKind = Fn of FunctionItem


type Item =
    { Id: NodeId
      Span: Span
      Kind: ItemKind }


type Expr =
    { Id: NodeId
      Span: Span
      Kind: ExprKind }

and ExprKind =
    | ExprNum of int
    | ExprAdd of Expr * Expr


let rec eval expr =
    match expr.Kind with
    | ExprNum n -> n
    | ExprAdd(l, r) -> eval l + eval r
