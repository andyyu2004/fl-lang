module Ast

open Lex
open Format
open Span

type NodeId =
    { Id: int }

type PathSegment =
    { Ident: Ident }

    interface IShow with
        member this.Show() = show this.Ident


type Path =
    { Span: Span
      Id: NodeId
      Segments: list<PathSegment> }
    interface IShow with
        member this.Show() = showList this.Segments "::"


type LitKind =
    | LitInt of int
    | LitBool of bool
    interface IShow with
        member this.Show() =
            match this with
            | LitInt i -> sprintf "%d" i
            | LitBool b -> sprintf "%b" b

type Lit =
    { Span: Span
      Kind: LitKind }
    interface IShow with
        member this.Show() = show this.Kind

type UnOp =
    | UnOpNeg
    | UnOpNot

    static member FromToken =
        function
        | TkMinus -> UnOpNeg
        | TkBang -> UnOpNot
        | _ -> failwith "invalid unop"

    interface IShow with
        member this.Show() =
            match this with
            | UnOpNeg -> "-"
            | UnOpNot -> "!"

type BinOp =
    | BinOpAdd
    | BinOpSub
    | BinOpMul
    | BinOpDiv

    static member FromToken =
        function
        | TkPlus -> BinOpAdd
        | TkMinus -> BinOpSub
        | TkStar -> BinOpMul
        | TkSlash -> BinOpDiv
        | _ -> failwith "invalid binop"

    interface IShow with
        member this.Show() =
            match this with
            | BinOpAdd -> "+"
            | BinOpSub -> "-"
            | BinOpMul -> "*"
            | BinOpDiv -> "/"


[<RequireQualifiedAccess>]
type ExprKind =
    | Lit of Lit
    | Group of Expr
    | Path of Path
    | Unary of UnOp * Expr
    | Bin of BinOp * Expr * Expr
    | Tuple of list<Expr>
    | App of Expr * Expr


    interface IShow with
        member this.Show() =
            match this with
            | Lit lit -> sprintf "%s" (show lit)
            | Path path -> sprintf "%s" (show path)
            | Group expr -> sprintf "(%s)" (show expr)
            | Unary(op, expr) -> sprintf "(%s%s)" (show op) (show expr)
            | Bin(op, l, r) -> sprintf "(%s %s %s)" (show l) (show op) (show r)
            | Tuple(xs) -> sprintf "(%s)" (showList xs ",")
            | App(f, x) -> sprintf "(%s %s)" (show f) (show x)

and Expr =
    { Id: NodeId
      Span: Span
      Kind: ExprKind }

    interface IShow with
        member this.Show() = show this.Kind


[<RequireQualifiedAccess>]
type PatKind =
    | Bind of Ident
    | Group of Pat
    | Tuple of list<Pat>

    interface IShow with
        member this.Show() =
            match this with
            | Bind name -> show name
            | Group pat -> sprintf "(%s)" (show pat)
            | Tuple pats -> sprintf "(%s)" (showList pats ",")

and Pat =
    { Id: NodeId
      Span: Span
      Kind: PatKind }

    interface IShow with
        member this.Show() = show this.Kind

(* ast representation of types; not to be confused with `Ty` *)
// todo

[<RequireQualifiedAccess>]
type AstTyKind =
    | Int
    | Bool
    | Tuple of list<AstTy>
    | Path of Path
    | Fn of AstTy * AstTy

    interface IShow with
        member this.Show() =
            match this with
            | Int -> "int"
            | Bool -> "bool"
            | Tuple tys -> sprintf "(%s)" (showList tys ",")
            | Path path -> (path :> IShow).Show()
            | Fn(t, u) -> sprintf "(%s -> %s)" (show t) (show u)

and AstTy =
    { Id: NodeId
      Span: Span
      Kind: AstTyKind }

    interface IShow with
        member this.Show() = show this.Kind

(* type signature *)
type FnSig =
    { Ident: Ident
      Type: AstTy }

    interface IShow with
        member this.Show() = sprintf "sig %s :: %s" (show this.Ident) (show this.Type)

type FnDef =
    { Ident: Ident
      Params: list<Pat>
      Body: Expr }
    interface IShow with
        member this.Show() =
            // need two cases to get the spacing correct
            if this.Params.IsEmpty
            then sprintf "let %s = %s\n" (show this.Ident) (show this.Body)
            else sprintf "let %s %s = %s\n" (show this.Ident) (showList this.Params " ") (show this.Body)



[<RequireQualifiedAccess>]
type ItemKind =
    | FnDef of FnDef
    | Sig of FnSig

    interface IShow with
        member this.Show() =
            match this with
            | FnDef def -> show def
            | Sig fnsig -> show fnsig


type Item =
    { Id: NodeId
      Span: Span
      Kind: ItemKind }

    interface IShow with
        member this.Show() = show this.Kind


type Ast =
    { Items: list<Item> }

    interface IShow with
        member this.Show() = showList this.Items "\n"
