module AstVisit

open Span
open Ast
open RState

[<AbstractClass>]
type AstVisitor<'s, 'e>() =

    abstract VisitAst: Ast -> RState<'s, 'e, unit>

    default this.VisitAst ast = this.WalkAst ast

    member this.WalkAst ast = mapM' this.VisitItem ast.Items


    abstract VisitItem: Item -> RState<'s, 'e, unit>

    default this.VisitItem item = this.WalkItem item

    member this.WalkItem item =
        rstate {
            match item.Kind with
            | ItemKind.FnDef def -> do! this.VisitFnDef def
            | ItemKind.Sig fnsig -> do! this.VisitFnSig fnsig
        }

    abstract VisitFnSig: FnSig -> RState<'s, 'e, unit>

    default this.VisitFnSig fnsig = this.WalkFnSig fnsig

    member this.WalkFnSig fnsig =
        rstate {
            do! this.VisitIdent fnsig.Ident
            do! this.VisitTy fnsig.Type
        }

    abstract VisitFnDef: FnDef -> RState<'s, 'e, unit>

    default this.VisitFnDef def = this.WalkFnDef def

    member this.WalkFnDef def =
        rstate {
            do! this.VisitIdent def.Ident
            do! mapM' this.VisitPat def.Params
            do! this.VisitExpr def.Body
        }

    abstract VisitPat: Pat -> RState<'s, 'e, unit>


    default this.VisitPat pat = this.WalkPat pat

    member this.WalkPat pat =
        rstate {
            match pat.Kind with
            | PatKind.Bind ident -> do! this.VisitIdent ident
            | PatKind.Tuple(pats) -> do! mapM' this.VisitPat pats
        }

    abstract VisitTy: AstTy -> RState<'s, 'e, unit>

    default this.VisitTy ty =
        rstate {
            match ty.Kind with
            | AstTyKind.Bool
            | AstTyKind.Int -> return ()
            | AstTyKind.Tuple(tys) -> do! mapM' this.VisitTy tys
            | AstTyKind.Path(path) -> do! this.VisitPath path
            | AstTyKind.Fn(param, ret) ->
                do! this.VisitTy param
                do! this.VisitTy ret
        }

    abstract VisitExpr: Expr -> RState<'s, 'e, unit>

    default this.VisitExpr expr = this.WalkExpr expr

    member this.WalkExpr expr =
        rstate {
            match expr.Kind with
            | ExprKind.Path(path) -> do! this.VisitPath path
            | ExprKind.Tuple(exprs) -> do! mapM' this.VisitExpr exprs
            | ExprKind.Lit(_lit) -> return ()
            | ExprKind.Unary(_, expr) -> do! this.VisitExpr expr
            | ExprKind.Let(pat, expr, body) ->
                do! this.VisitPat pat
                do! this.VisitExpr expr
                do! this.VisitExpr body
            | ExprKind.App(lhs, rhs)
            | ExprKind.Bin(_, lhs, rhs) ->
                do! this.VisitExpr lhs
                do! this.VisitExpr rhs
            | ExprKind.Fn(pats, body) ->
                do! mapM' this.VisitPat pats
                do! this.VisitExpr body
        }

    abstract VisitIdent: Ident -> RState<'s, 'e, unit>
    default _this.VisitIdent _ident = rstate { return () }

    abstract VisitPathSegment: PathSegment -> RState<'s, 'e, unit>
    default this.VisitPathSegment segment = this.VisitIdent segment.Ident

    abstract VisitPath: Path -> RState<'s, 'e, unit>
    default this.VisitPath path = mapM' this.VisitPathSegment path.Segments
