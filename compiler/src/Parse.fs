module Parse

open Result
open RState
open Span
open Format
open Ast
open Lex

type ParseError =
    | ParseError

    interface IShow with
        member _this.Show() = "parse error"


type ParseCtxt =
    { Tokens: list<Token>
      IdCounter: int }


type Parse<'a> = RState<ParseCtxt, ParseError, 'a>

let runParser (R f) tokens = f tokens

let (>>=) x f = bind f x

let private error err = R(fun pcx -> (Error err, pcx))


let parse = rstate


let (>*>) p q =
    parse {
        let! _ = p
        return! q }

let rec private many1 p: Parse<list<'a>> =
    parse {
        let! x = p
        let! xs = many p
        return x :: xs }

and private many p: Parse<list<'a>> = many1 p <|> parse.Return []

/// parsers `p` if it can; always returns ()
let optional p: Parse<unit> =
    parse {
        let! _ = p
        return () } <|> parse { return () }


let rec private sepBy p sep = sepBy1 p sep <|> parse.Return []

and private sepBy1 p sep =
    parse {
        let! x = p
        let! xs = many (sep >*> p)
        do! optional sep
        // parse trailing separator
        return x :: xs
    }

let rec private sequence =
    function
    | [] -> parse.Return []
    | p :: ps ->
        parse {
            let! x = p
            let! xs = sequence ps
            return x :: xs }

let private map f p =
    parse {
        let! x = p
        return f x }

(* parsing logic *)


let private next: Parse<Token> =
    parse {
        let! pcx = get
        match pcx.Tokens with
        | [] -> return! error ParseError
        | t :: ts ->
            do! put { pcx with Tokens = ts }
            return t
    }


let private accept kind: Parse<option<Token>> =
    parse {
        let! pcx = get
        match pcx.Tokens with
        | [] -> return None
        | t :: ts ->
            if t.Kind = kind then
                do! put { pcx with Tokens = ts }
                return Some t
            else
                return None
    }


let rec private acceptOneOf kinds: Parse<option<Token>> =
    parse {
        match kinds with
        | [] -> return None
        | kind :: kinds ->
            match! accept kind with
            | Some token -> return Some token
            | None -> return! acceptOneOf kinds
    }

let private expect kind: Parse<Token> =
    parse {
        match! accept kind with
        | Some token -> return token
        | None -> return! error ParseError
    }

let private nextId: Parse<NodeId> =
    parse {
        let! pcx = get
        let idx = pcx.IdCounter
        do! put { pcx with IdCounter = 1 + idx }
        return { Id = idx }
    }

let private acceptIdent: Parse<option<Ident>> =
    parse {
        let! token = next
        match token.Kind with
        | TkIdent ident -> return Some ident
        | _ -> return None
    }

let rec private parseToken tk =
    parse {
        let! _ = expect tk
        return () }

let private parseTuple p = parse { return! sepBy p <| parseToken TkComma }

let private expectIdent =
    parse {
        match! acceptIdent with
        | Some ident -> return ident
        | None -> return! error ParseError
    }

let private acceptInt: Parse<option<Span * int>> =
    parse {
        let! token = next
        match token.Kind with
        | TkInt i -> return Some(token.Span, i)
        | _ -> return None
    }

let private expectInt =
    parse {
        match! acceptInt with
        | Some i -> return i
        | None -> return! error ParseError
    }

let private mkTy span kind: Parse<AstTy> =
    parse {
        let! idx = nextId
        return { Id = idx
                 Span = span
                 Kind = kind }
    }

let private parsePathSegment: Parse<PathSegment> =
    parse {
        let! ident = expectIdent
        return { Ident = ident } }

let private mkPath span segments =
    parse {
        let! idx = nextId
        return { Span = span
                 Segments = segments
                 Id = idx }
    }

let private parsePath =
    parse {
        let! segment = parsePathSegment
        let span = segment.Ident.Span
        return! mkPath span [ segment ]
    }

let private parseTyPath =
    parse {
        let! path = parsePath
        let kind = AstTyKind.Path path
        return! mkTy (path.Span) kind
    }

#nowarn "40"


let rec private parseTy: Parse<AstTy> =
    parse {
        // use the <|> combinator to parse all the potential different types
        let! ty = parseTyPath <|> parseGroupTy <|> parseTupleTy
        match! accept TkRArrow with
        | None -> return ty
        | Some _ ->
            let! rty = parseTy
            let span = ty.Span ++ rty.Span
            let kind = AstTyKind.Fn(ty, rty)
            return! mkTy span kind
    }

and parseTupleTy =
    parse {
        let! l = expect TkLParen
        let! tys = parseTuple parseTy
        let! r = expect TkRParen
        let span = l.Span ++ r.Span
        let kind = AstTyKind.Tuple tys
        return! mkTy span kind
    }

and parseGroupTy =
    parse {
        let! l = expect TkLParen
        let! ty = parseTy
        let! r = expect TkRParen
        let span = l.Span ++ r.Span
        return { ty with Span = span }
    }

let rec mkExpr span kind: Parse<Expr> =
    parse {
        let! idx = nextId
        return { Id = idx
                 Span = span
                 Kind = kind }
    }

let private parseLiteralInt: Parse<Lit> =
    parse {
        let! (span, i) = expectInt
        return { Span = span
                 Kind = LitInt i }
    }

let rec private parseAssoc' ops (p: Parse<Expr>) (l: Expr): Parse<Expr> =
    parse {
        let! token = acceptOneOf ops
        if Option.isSome token then
            let binop = token.Value.Kind |> BinOp.FromToken
            let! r = p
            let span = l.Span ++ r.Span
            let kind = ExprKind.Bin(binop, l, r)
            let! expr = mkExpr span kind
            return! parseAssoc' ops p expr
        else
            return l
    }

let private parseAssoc ops p =
    parse {
        let! left = p
        return! parseAssoc' ops p left }


let private mkPat span kind: Parse<Pat> =
    parse {
        let! idx = nextId
        return { Id = idx
                 Span = span
                 Kind = kind }
    }

let rec private parsePat: Parse<Pat> = parsePatBind <|> parsePatGroup <|> parsePatTuple

and parsePatBind =
    parse {
        let! name = expectIdent
        let kind = PatKind.Bind name
        return! mkPat name.Span kind
    }

and parsePatGroup =
    parse {
        let! l = expect TkLParen
        let! pat = parsePat
        let! r = expect TkRParen
        let span = l.Span ++ r.Span
        return { pat with Span = span }
    }

and parsePatTuple =
    parse {
        let! l = expect TkLParen
        let! pats = parseTuple parsePat
        let! r = expect TkRParen
        let span = l.Span ++ r.Span
        let kind = PatKind.Tuple pats
        return! mkPat span kind
    }


let private parseExprPath =
    parse {
        let! path = parsePath
        let kind = ExprKind.Path path
        return! mkExpr (path.Span) kind
    }

let rec private parseExpr: Parse<Expr> = parseTerm

and parseTerm = parseAssoc [ TkPlus; TkMinus ] parseFactor

and parseFactor = parseAssoc [ TkStar; TkSlash ] parseUnary

and parseUnary =
    parse {
        let! token = acceptOneOf [ TkMinus; TkBang ]
        match token with
        | None -> return! parseApp
        | Some token ->
            let unop = UnOp.FromToken token.Kind
            let! expr = parseUnary
            let span = token.Span ++ expr.Span
            let kind = ExprKind.Unary(unop, expr)
            return! mkExpr span kind
    }

and parseApp =
    parse {
        let! expr = parsePrimary
        return! parseApp' expr }

// tries to parse as many expressions until an error
and parseApp' left =
    let expr =
        parse {
            let! right = parsePrimary
            let span = left.Span ++ right.Span
            let kind = ExprKind.App(left, right)
            let! expr = mkExpr span kind
            return! parseApp' expr
        }
    catch expr left

// note we must parse group before tuple as `( <expr> )` should be parsed as a group not a tuple
and parsePrimary: Parse<Expr> = parseExprLambda <|> parseExprGroup <|> parseExprTuple <|> parseExprPath <|> parseExprLit

and parseExprLambda =
    parse {
        let! fn = expect TkFn
        let! pats = many parsePat
        do! parseToken TkRArrow
        let! body = parseExpr
        let kind = ExprKind.Fn(pats, body)
        let span = fn.Span ++ body.Span
        return! mkExpr span kind
    }

and parseExprTuple =
    parse {
        let! l = expect TkLParen
        let! exprs = parseTuple parseExpr
        let! r = expect TkRParen
        let span = l.Span ++ r.Span
        let kind = ExprKind.Tuple exprs
        return! mkExpr span kind
    }

and parseExprGroup =
    parse {
        let! l = expect TkLParen
        let! expr = parseExpr
        let! r = expect TkRParen
        let span = l.Span ++ r.Span
        return { expr with Span = span }
    }

and parseExprLit =
    parse {
        (* let! lit = (parseLiteralInt <|> parseLiteralBool) *)
        let! lit = parseLiteralInt
        let kind = ExprKind.Lit lit

        return! mkExpr lit.Span kind
    }



(* let private parseFnItem = parse {  } *)

let private parseFnDef =
    parse {
        let! let_kw = expect TkLet
        let! ident = expectIdent
        let! pats = many parsePat
        do! parseToken TkEq
        let! body = parseExpr
        let span = let_kw.Span ++ body.Span

        let def =
            { Ident = ident
              Params = pats
              Body = body }

        return (span, ItemKind.FnDef def)
    }

let private parseFnSig =
    parse {
        let! sig_kw = expect TkSig
        let! ident = expectIdent
        do! parseToken TkDColon
        let! ty = parseTy
        let span = sig_kw.Span ++ ty.Span

        let fnsig =
            { Ident = ident
              Type = ty }
        return (span, ItemKind.Sig fnsig)
    }


let private mkItem span kind: Parse<Item> =
    parse {
        let! idx = nextId
        return { Id = idx
                 Span = span
                 Kind = kind }
    }

/// <item> = <function-item>
///
// maybe add a keyword for new itemkinds if we want to add some
// such as `const` etc..
// if there is no such keyword, assume it is a function definition
//
/// <function-item> =
///     <ident> :: <type>
///     <ident> <arg>* = <expr>
///
/// f :: Int -> Int
/// f x y = x + y

let private parseItem: Parse<Item> =
    parse {
        let! (span, kind) = parseFnDef <|> parseFnSig
        return! mkItem span kind }

(* top level ast parse function *)
let private parseProgramInner: Parse<Ast> =
    parse {
        let! items = many parseItem
        return { Items = items } }

let parseProgram tokens =
    runParser parseProgramInner
        { Tokens = tokens
          IdCounter = 0 }
    |> fst
