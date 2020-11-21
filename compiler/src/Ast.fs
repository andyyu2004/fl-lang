module Ast

open Lex
open Format

type NodeId =
    { Id: int }

type PathSegment =
    { Ident: Ident }

    interface IShow with
        member this.Show() = show this.Ident


type Path =
    { Span: Span
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

type Expr =
    { Id: NodeId
      Span: Span
      Kind: ExprKind }

    interface IShow with
        member this.Show() = show this.Kind

and ExprKind =
    | ExprLit of Lit
    | ExprGroup of Expr
    | ExprPath of Path
    | ExprUnary of UnOp * Expr
    | ExprBin of BinOp * Expr * Expr
    | ExprTuple of list<Expr>

    interface IShow with
        member this.Show() =
            match this with
            | ExprLit lit -> sprintf "%s" (show lit)
            | ExprPath path -> sprintf "%s" (show path)
            | ExprGroup expr -> sprintf "(%s)" (show expr)
            | ExprUnary(op, expr) -> sprintf "(%s%s)" (show op) (show expr)
            | ExprBin(op, l, r) -> sprintf "(%s %s %s)" (show l) (show op) (show r)
            | ExprTuple(xs) -> sprintf "(%s)" (showList xs ",")

type Pat =
    { Id: NodeId
      Span: Span
      Kind: PatKind }

and PatKind =
    | PatBind of Ident
    | PatGroup of Pat

(* ast representation of types; not to be confused with `Ty` *)
// todo
type Type =
    { Id: NodeId
      Span: Span
      Kind: TypeKind }

    interface IShow with
        member this.Show() = show this.Kind

and TypeKind =
    | AstTyInt
    | AstTyBool
    | AstTyTuple of list<Type>
    | AstTyPath of Path
    | AstTyFn of Type * Type

    interface IShow with
        member this.Show() =
            match this with
            | AstTyInt -> "int"
            | AstTyBool -> "bool"
            | AstTyTuple tys -> sprintf "(%s)" (showList tys ",")
            | AstTyPath path -> (path :> IShow).Show()
            | AstTyFn(t, u) -> sprintf "(%s -> %s)" (show t) (show u)


(* type signature *)
type Sig = Type

type FnDef =
    { Ident: Ident
      Span: Span
      Params: list<unit>
      Body: Expr }
    interface IShow with
        member this.Show() = sprintf "%s = %s" (show this.Ident) (show this.Body)

type FnItem =
    { Ident: Ident
      Span: Span
      Sig: Sig
      Def: FnDef }
    interface IShow with
        member this.Show() =
            sprintf "%s :: %s\n%s" (show this.Ident) (show this.Sig) (show this.Def)


type ItemKind =
    | ItemFn of FnItem

    interface IShow with
        member this.Show() =
            match this with
            | ItemFn fn -> show fn


type Item =
    { Id: NodeId
      Span: Span
      Kind: ItemKind }

    interface IShow with
        member this.Show() = show this.Kind


type Ast =
    { Items: list<Item> }

    interface IShow with
        member this.Show() = showList this.Items "\n\n"
