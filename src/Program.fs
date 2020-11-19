module Main

open Parse
open System.IO


type Span =
    { Lo: int
      Hi: int }

type Expr =
    { Span: Span
      Kind: ExprKind }

and ExprKind =
    | ExprNum of int
    | ExprAdd of Expr * Expr


let rec eval expr =
    match expr.Kind with
    | ExprNum n -> n
    | ExprAdd(l, r) -> eval (l) + eval (r)


[<EntryPoint>]
let main argv = 0
