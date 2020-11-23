module Ast

open Lex
open Format
open Span

type NodeId =
    { Id: int }

    override this.ToString() = show this

    interface IShow with
        member this.Show() = sprintf "%%%d" this.Id

type PathSegment =
    { Ident: Ident }

    override this.ToString() = show this

    interface IShow with
        member this.Show() = show this.Ident


type Path =
    { Span: Span
      Id: NodeId
      Segments: list<PathSegment> }
    override this.ToString() = show this
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

    override this.ToString() = show this
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

    override this.ToString() = show this
    interface IShow with
        member this.Show() =
            match this with
            | BinOpAdd -> "+"
            | BinOpSub -> "-"
            | BinOpMul -> "*"
            | BinOpDiv -> "/"


[<RequireQualifiedAccess>]
type PatKind =
    | Bind of Ident
    | Tuple of list<Pat>

    override this.ToString() = show this
    interface IShow with
        member this.Show() =
            match this with
            | Bind name -> show name
            | Tuple pats -> sprintf "(%s)" (showList pats ",")

and Pat =
    { Id: NodeId
      Span: Span
      Kind: PatKind }

    override this.ToString() = show this
    interface IShow with
        member this.Show() = show this.Kind

[<RequireQualifiedAccess>]
type ExprKind =
    | Path of Path
    | Tuple of list<Expr>
    | Lit of Lit
    | Unary of UnOp * Expr
    | Bin of BinOp * Expr * Expr
    | App of Expr * Expr
    | Fn of list<Pat> * Expr


    override this.ToString() = show this
    interface IShow with
        member this.Show() =
            match this with
            | Lit lit -> sprintf "%s" (show lit)
            | Path path -> sprintf "%s" (show path)
            | Unary(op, expr) -> sprintf "(%s%s)" (show op) (show expr)
            | Bin(op, l, r) -> sprintf "(%s %s %s)" (show l) (show op) (show r)
            | Tuple(xs) -> sprintf "(%s)" (showTuple xs)
            | App(f, x) -> sprintf "(%s %s)" (show f) (show x)
            | Fn(args, body) -> sprintf "(fn %s -> %s)" (showTuple args) (show body)

and Expr =
    { Id: NodeId
      Span: Span
      Kind: ExprKind }

    interface IShow with
        member this.Show() = show this.Kind

    interface ISpanned with
        member this.GetSpan = this.Span



(* ast representation of types; not to be confused with `Ty` *)
[<RequireQualifiedAccess>]
type AstTyKind =
    | Int
    | Bool
    | Tuple of list<AstTy>
    | Path of Path
    | Fn of AstTy * AstTy

    override this.ToString() = show this
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

    override this.ToString() = show this
    interface IShow with
        member this.Show() = show this.Kind

(* type signature *)
type FnSig =
    { Ident: Ident
      Type: AstTy }

    override this.ToString() = show this
    interface IShow with
        member this.Show() = sprintf "sig %s :: %s" (show this.Ident) (show this.Type)

type FnDef =
    { Ident: Ident
      Params: list<Pat>
      Body: Expr }
    override this.ToString() = show this
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

    override this.ToString() = show this
    interface IShow with
        member this.Show() =
            match this with
            | FnDef def -> show def
            | Sig fnsig -> show fnsig


type Item =
    { Id: NodeId
      Span: Span
      Kind: ItemKind }

    override this.ToString() = show this

    interface IShow with
        member this.Show() = show this.Kind

    interface ISpanned with
        member this.GetSpan = this.Span


type Ast =
    { Items: list<Item> }

    override this.ToString() = show this

    interface IShow with
        member this.Show() = showList this.Items "\n"
