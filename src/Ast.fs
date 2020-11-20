module Ast

open Lex
open Format

type NodeId =
    { Id: int }

(* ast representation of types *)

type PathSegment =
    { Ident: Ident }

    interface IShow with
        member this.Show() = show this.Ident


type Path =
    { Segments: list<PathSegment> }
    interface IShow with
        member this.Show() = showList this.Segments "::"

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

(* type signature *)
type Sig = Type

type FunctionItem =
    { Ident: Ident
      Sig: Sig }
    interface IShow with
        member this.Show() = sprintf "%s :: %s" (show this.Ident) (show this.Sig)


type ItemKind =
    | ItemFn of FunctionItem

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


type Expr =
    { Id: NodeId
      Span: Span
      Kind: ExprKind }

and ExprKind =
    | ExprNum of int
    | ExprAdd of Expr * Expr

type Ast =
    { Items: list<Item> }

    interface IShow with
        member this.Show() = showList this.Items "\n\n"
