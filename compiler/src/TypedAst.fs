module TypedAst

/// items in this module are similar to Ast but prefixed with a T (for type)

open Type
open Span
open Format
open Ast

[<RequireQualifiedAccess>]
type TPatKind =
    | Bind of Ident
    | Tuple of list<TPat>

    override this.ToString() = show this
    interface IShow with
        member this.Show() =
            match this with
            | Bind name -> show name
            | Tuple pats -> sprintf "(%s)" (showList pats ",")

and TPat =
    { Id: NodeId
      Span: Span
      Ty: Ty
      Kind: TPatKind }

    override this.ToString() = show this
    interface IShow with
        member this.Show() = sprintf "%s:%s" (show this.Kind) (show this.Ty)

[<RequireQualifiedAccess>]
type TExprKind =
    | Var of NodeId
    | Tuple of list<Expr>
    | Lit of Lit
    | Let of TPat * TExpr * TExpr
    | Unary of UnOp * TExpr
    | Bin of BinOp * TExpr * TExpr
    | App of TExpr * TExpr
    | Fn of list<TPat> * TExpr


    override this.ToString() = show this
    interface IShow with
        member this.Show() =
            match this with
            | Lit lit -> sprintf "%s" (show lit)
            | Var idx -> sprintf "%s" (show idx)
            | Unary(op, expr) -> sprintf "(%s%s)" (show op) (show expr)
            | Bin(op, l, r) -> sprintf "(%s %s %s)" (show l) (show op) (show r)
            | Tuple(xs) -> sprintf "(%s)" (showList xs ",")
            | App(f, x) -> sprintf "(%s %s)" (show f) (show x)
            | Fn(pats, body) -> sprintf "(fn %s -> %s)" (showList pats " ") (show body)
            | Let(pat, expr, body) ->
                sprintf "let %s = %s in\n%s" (show pat) (show expr) (show body)

and TExpr =
    { Id: NodeId
      Span: Span
      Ty: Ty
      Kind: TExprKind }

    interface IShow with
        member this.Show() = sprintf "%s:%s" (show this.Kind) (show this.Ty)

    interface ISpanned with
        member this.GetSpan = this.Span



type TFnDef =
    { Ident: Ident
      Params: list<TPat>
      Body: TExpr }
    override this.ToString() = show this
    interface IShow with
        member this.Show() =
            // need two cases to get the spacing correct
            if this.Params.IsEmpty
            then sprintf "let %s = %s\n" (show this.Ident) (show this.Body)
            else sprintf "let %s %s = %s\n" (show this.Ident) (showList this.Params " ") (show this.Body)

[<RequireQualifiedAccess>]
type TItemKind =
    | FnDef of TFnDef

    override this.ToString() = show this
    interface IShow with
        member this.Show() =
            match this with
            | FnDef def -> show def


type TItem =
    { Id: NodeId
      Span: Span
      Kind: TItemKind }

    override this.ToString() = show this

    interface IShow with
        member this.Show() = show this.Kind

    interface ISpanned with
        member this.GetSpan = this.Span


type TAst =
    { Items: list<TItem> }

    override this.ToString() = show this

    interface IShow with
        member this.Show() = showList this.Items "\n"
