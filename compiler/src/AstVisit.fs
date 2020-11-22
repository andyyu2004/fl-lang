module AstVisit

open Lex
open Ast

type IAstVisitor =
    abstract VisitAst: Ast -> unit

    default this.VisitAst ast =
        for item in ast.Items do
            this.VisitItem item

    abstract VisitItem: Item -> unit

    default this.VisitItem item =
        match item.Kind with
        | ItemKind.Fn fn ->
            this.VisitIdent fn.Ident
            this.VisitFnSig fn.Sig
            this.VisitFnDef fn.Def

    abstract VisitFnSig: Sig -> unit
    default this.VisitFnSig fnsig = this.VisitTy fnsig

    abstract VisitFnDef: FnDef -> unit

    default this.VisitFnDef def =
        this.VisitIdent def.Ident
        for param in def.Params do
            this.VisitPat param
        this.VisitExpr def.Body

    abstract VisitPat: Pat -> unit

    default this.VisitPat pat =
        match pat.Kind with
        | PatKind.Bind ident -> this.VisitIdent ident
        | PatKind.Group(pat) -> this.VisitPat pat
        | PatKind.Tuple(pats) ->
            for pat in pats do
                this.VisitPat pat

    abstract VisitTy: AstTy -> unit

    default this.VisitTy ty =
        match ty.Kind with
        | AstTyKind.Bool
        | AstTyKind.Int -> ()
        | AstTyKind.Tuple(tys) ->
            for ty in tys do
                this.VisitTy ty
        | AstTyKind.Path(path) -> this.VisitPath path
        | AstTyKind.Fn(param, ret) ->
            this.VisitTy param
            this.VisitTy ret

    abstract VisitExpr: Expr -> unit

    default this.VisitExpr expr =
        match expr.Kind with
        | ExprKind.Group expr -> this.VisitExpr expr
        | ExprKind.Lit(_lit) -> ()
        | ExprKind.Path(path) -> this.VisitPath path
        | ExprKind.Unary(_, expr) -> this.VisitExpr expr
        | ExprKind.Bin(_, lhs, rhs) ->
            this.VisitExpr lhs
            this.VisitExpr rhs
        | ExprKind.Tuple(exprs) ->
            for expr in exprs do
                this.VisitExpr expr

    abstract VisitIdent: Ident -> unit
    default _this.VisitIdent _ident = ()

    abstract VisitPathSegment: PathSegment -> unit
    default this.VisitPathSegment segment = this.VisitIdent segment.Ident

    abstract VisitPath: Path -> unit
    default this.VisitPath path =
        for segment in path.Segments do
            this.VisitPathSegment segment
