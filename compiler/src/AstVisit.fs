module AstVisit

open Span
open Ast
open State

[<AbstractClass>]
type AstVisitor<'s>() =

    abstract VisitAst: Ast -> State<'s, unit>

    default this.VisitAst ast = this.WalkAst ast

    member this.WalkAst ast = mapM' this.VisitItem ast.Items


    abstract VisitItem: Item -> State<'s, unit>

    default this.VisitItem item = this.WalkItem item

    member this.WalkItem item =
        state {
            match item.Kind with
            | ItemKind.FnDef def -> do! this.VisitFnDef def
            | ItemKind.Sig fnsig -> do! this.VisitFnSig fnsig
        }

    abstract VisitFnSig: FnSig -> State<'s, unit>

    default this.VisitFnSig fnsig =
        state {
            do! this.VisitIdent fnsig.Ident
            do! this.VisitTy fnsig.Type
        }

    abstract VisitFnDef: FnDef -> State<'s, unit>

    default this.VisitFnDef def = this.WalkFnDef def

    member this.WalkFnDef def =
        state {
            do! this.VisitIdent def.Ident
            do! mapM' this.VisitPat def.Params
            do! this.VisitExpr def.Body
        }

    abstract VisitPat: Pat -> State<'s, unit>


    default this.VisitPat pat = this.WalkPat pat

    member this.WalkPat pat =
        state {
            match pat.Kind with
            | PatKind.Bind ident -> do! this.VisitIdent ident
            | PatKind.Tuple(pats) -> do! mapM' this.VisitPat pats
        }

    abstract VisitTy: AstTy -> State<'s, unit>

    default this.VisitTy ty =
        state {
            match ty.Kind with
            | AstTyKind.Bool
            | AstTyKind.Int -> return ()
            | AstTyKind.Tuple(tys) -> do! mapM' this.VisitTy tys
            | AstTyKind.Path(path) -> do! this.VisitPath path
            | AstTyKind.Fn(param, ret) ->
                do! this.VisitTy param
                do! this.VisitTy ret
        }

    abstract VisitExpr: Expr -> State<'s, unit>

    default this.VisitExpr expr = this.WalkExpr expr

    member this.WalkExpr expr =
        state {
            match expr.Kind with
            | ExprKind.Path(path) -> do! this.VisitPath path
            | ExprKind.Lit(_lit) -> return ()
            | ExprKind.Unary(_, expr) -> do! this.VisitExpr expr
            | ExprKind.App(lhs, rhs)
            | ExprKind.Bin(_, lhs, rhs) ->
                do! this.VisitExpr lhs
                do! this.VisitExpr rhs
            | ExprKind.Tuple(exprs) -> do! mapM' this.VisitExpr exprs
        }

    abstract VisitIdent: Ident -> State<'s, unit>
    default _this.VisitIdent _ident = state { return () }

    abstract VisitPathSegment: PathSegment -> State<'s, unit>
    default this.VisitPathSegment segment = this.VisitIdent segment.Ident

    abstract VisitPath: Path -> State<'s, unit>
    default this.VisitPath path = mapM' this.VisitPathSegment path.Segments
