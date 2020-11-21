module Ast

open Lex
open Format

type NodeId =
    { Id: int }

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
    | ExprBin of BinOp * Expr * Expr

    interface IShow with
        member this.Show() =
            match this with
            | ExprLit lit -> sprintf "%s" (show lit)
            | ExprBin(op, l, r) -> sprintf "(%s %s %s)" (show l) (show op) (show r)

type PathSegment =
    { Ident: Ident }

    interface IShow with
        member this.Show() = show this.Ident


type Path =
    { Segments: list<PathSegment> }
    interface IShow with
        member this.Show() = showList this.Segments "::"


(* ast representation of types; not to be confused with `Ty` *)
type Type =
    | AstTyInt
    | AstTyBool
    | AstTyPath of Path
    | AstTyFn of Type * Type

    interface IShow with
        member this.Show() =
            match this with
            | AstTyInt -> "int"
            | AstTyBool -> "bool"
            | AstTyPath path -> (path :> IShow).Show()
            | AstTyFn(t, u) -> sprintf "(%s -> %s)" (show t) (show u)

// todo
type TypeKind =
    { Kind: Type }

(* type signature *)
type Sig = Type

type FnDef =
    { Ident: Ident
      Span: Span
      Params: list<unit>
      Body: Expr }
    interface IShow with
        member this.Show() = sprintf "%s %s = %s" (show this.Ident) "<params>" (show this.Body)

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
