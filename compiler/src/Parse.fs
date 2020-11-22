module Parse

open Result
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


(* note the parsectxt is outside the result, so is returned on both error and success *)
type Parse<'a> = P of (ParseCtxt -> (Result<'a, ParseError> * ParseCtxt))

let runParser (P f) tokens = f tokens

let (>>=) x f = bind f x

let private error err = P(fun pcx -> (Error err, pcx))


module private Parsers =
    let ret t = P(fun pcx -> (Ok t, pcx))

    let bind (P p) f =
        P(fun pcx ->
                let (r, pcx') = p pcx
                match r with
                | Ok t -> runParser (f t) pcx'
                | Error err -> (Error err, pcx'))

    (* >>= (fun (t, pcx') -> let (P q) = f t in q pcx')) *)


    (* sequence (>>) in haskell *)
    let combine p q = bind p (fun _ -> q)

    let zero() = P(fun ctx -> (Error ParseError, ctx))


type ParserBuilder() =
    member _x.Return t = Parsers.ret t
    member _x.ReturnFrom(p) = p
    member _x.Bind(t, f) = Parsers.bind t f
    member _x.Zero() = Parsers.zero()
    member _x.Combine(p, q) = Parsers.combine p q
    member _x.Delay(f) =
        P(fun src -> let (P g) = f() in g src)

let private parse = ParserBuilder()

let (<*>) f x =
    parse {
        let! f = f
        let! x = x
        return f x }

(* combinators *)


(* let integer = P(fun src -> ) *)


/// the `or` parser combinator
// note the intentional choice return `q pctx` instead of `q pctx'`
// on error
// this allows this combinator to be used as a backtrack over multiple options
let (<|>) (P p) (P q) =
    P(fun pcx ->
            match p pcx with
            | (Ok a, pcx') -> (Ok a, pcx')
            | (Error _, _pctx') -> q pcx)

let (>*>) p q =
    parse {
        let! _ = p
        return! q }

let catch (P p) t =
    P(fun pcx ->
            match p pcx with
            | (Error _, _pcx) -> (Ok t, pcx)
            | (Ok a, pcx') -> (Ok a, pcx'))


let rec private many1 p: Parse<list<'a>> =
    parse {
        let! x = p
        let! xs = many p
        return x :: xs }

and private many p: Parse<list<'a>> = many1 p <|> Parsers.ret []

/// parsers `p` if it can; always returns ()
let optional p: Parse<unit> =
    parse {
        let! _ = p
        return () } <|> parse { return () }


let rec private sepBy p sep = sepBy1 p sep <|> parse { return [] }

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
    | [] -> Parsers.ret []
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

let get = P(fun pcx -> (Ok pcx, pcx))
let put pcx = P(fun _ -> (Ok(), pcx))


let private next: Parse<Token> =
    parse {
        let! pcx = get
        match pcx.Tokens with
        | [] -> return! error ParseError
        | t :: ts ->
            do! put { pcx with Tokens = ts }
            return t
    }


let private accept kind: Parse<Option<Token>> =
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


let rec private acceptOneOf kinds: Parse<Option<Token>> =
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

let private acceptIdent: Parse<Option<Ident>> =
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

let private acceptInt: Parse<Option<Span * int>> =
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
        let! ty = parseTyPath <|> parseTupleTy
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
and parsePrimary: Parse<Expr> = parseGroupExpr <|> parseTupleExpr <|> parseExprPath <|> parseLiteralExpr

and parseTupleExpr =
    parse {
        let! l = expect TkLParen
        let! exprs = parseTuple parseExpr
        let! r = expect TkRParen
        let span = l.Span ++ r.Span
        let kind = ExprKind.Tuple exprs
        return! mkExpr span kind
    }

and parseGroupExpr =
    parse {
        let! l = expect TkLParen
        let! expr = parseExpr
        let! r = expect TkRParen
        let span = l.Span ++ r.Span
        let kind = ExprKind.Group expr
        return! mkExpr span kind
    }

and parseLiteralExpr =
    parse {
        (* let! lit = (parseLiteralInt <|> parseLiteralBool) *)
        let! lit = parseLiteralInt
        let kind = ExprKind.Lit lit

        return! mkExpr lit.Span kind
    }


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
        let kind = PatKind.Group pat
        return! mkPat span kind
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
