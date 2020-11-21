module Parse

open Format
open Result
open Ast
open Lex


type ParseError = ParseError


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
    P(fun pctx ->
            match p pctx with
            | (Ok a, pctx') -> (Ok a, pctx')
            | (Error _, _pctx') -> q pctx)

let (>*>) p q =
    parse {
        let! _ = p
        return! q }


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


let private tokens =
    parse {
        let! pcx = get
        return pcx.Tokens }

let private currSpan =
    parse {
        let! t = tokens
        return t.[0].Span }

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

let private parseTuple p = parse { return! sepBy1 p <| parseToken TkComma }

let private expectIdent =
    parse {
        match! acceptIdent with
        | Some ident -> return ident
        | None -> return! error ParseError
    }

let private acceptInt: Parse<Option<int>> =
    parse {
        let! token = next
        match token.Kind with
        | TkInt i -> return Some i
        | _ -> return None
    }

let private expectInt =
    parse {
        match! acceptInt with
        | Some i -> return i
        | None -> return! error ParseError
    }

let private mkTy span kind: Parse<Type> =
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

let private parsePathTy =
    parse {
        let! segment = parsePathSegment
        let span = segment.Ident.Span

        let path =
            { Segments = [ segment ]
              Span = span }

        let kind = AstTyPath path
        return! mkTy span kind
    }

#nowarn "40"


let rec private parseTy: Parse<Type> =
    parse {
        // use the <|> combinator to parse all the potential different types
        let! ty = parsePathTy <|> parseTupleTy
        match! accept TkRArrow with
        | None -> return ty
        | Some _ ->
            let! rty = parseTy
            let span = ty.Span ++ rty.Span
            let kind = AstTyFn(ty, rty)
            return! mkTy span kind
    }

and parseTupleTy =
    parse {
        let! l = expect TkLParen
        let! tys = parseTuple parseTy
        let! r = expect TkRParen
        let span = l.Span ++ r.Span
        let kind = AstTyTuple tys
        return! mkTy span kind
    }

let rec mkExpr span kind: Parse<Expr> =
    parse {
        let! idx = nextId
        return { Id = idx
                 Span = span
                 Kind = kind }
    }

(* let private parseLiteralBool = failwith "" *)
let private parseLiteralInt: Parse<Lit> =
    parse {
        let! span = currSpan
        let! i = expectInt
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
            let kind = ExprBin(binop, l, r)
            let! expr = mkExpr span kind
            return! parseAssoc' ops p expr
        else
            return l
    }

let private parseAssoc ops p =
    parse {
        let! left = p
        return! parseAssoc' ops p left }



let rec private parseExpr: Parse<Expr> = parseTerm

and parseTerm = parseAssoc [ TkPlus; TkMinus ] parseFactor

and parseFactor = parseAssoc [ TkStar; TkSlash ] parseUnary

and parseUnary =
    parse {
        let! token = acceptOneOf [ TkMinus; TkBang ]
        match token with
        | Some token ->
            let unop = UnOp.FromToken token.Kind
            let! expr = parseUnary
            let span = token.Span ++ expr.Span
            let kind = ExprUnary(unop, expr)
            return! mkExpr span kind
        | None -> return! parsePrimary
    }

// note we must parse group before tuple as `( <expr> )` should be parsed as a group not a tuple
and parsePrimary: Parse<Expr> = parseLiteralExpr <|> parseGroupExpr <|> parseTupleExpr

and parseTupleExpr =
    parse {
        let! l = expect TkLParen
        let! exprs = parseTuple parseExpr
        let! r = expect TkRParen
        let span = l.Span ++ r.Span
        let kind = ExprTuple exprs
        return! mkExpr span kind
    }

and parseGroupExpr =
    parse {
        let! l = expect TkLParen
        let! expr = parseExpr
        let! r = expect TkRParen
        let span = l.Span ++ r.Span
        let kind = ExprGroup expr
        return! mkExpr span kind
    }

and parseLiteralExpr =
    parse {
        (* let! lit = (parseLiteralInt <|> parseLiteralBool) *)
        let! lit = parseLiteralInt
        let kind = ExprLit lit

        return! mkExpr lit.Span kind
    }

let private parseSig = parseTy

(* let private parseFnItem = parse {  } *)

let private parseFnDef =
    parse {
        let! ident = expectIdent
        // params
        do! parseToken TkEq
        let! body = parseExpr
        let span = ident.Span ++ body.Span
        return { Ident = ident
                 Span = span
                 Params = []
                 Body = body }
    }

let private parseFnItem =
    parse {
        let! ident = expectIdent
        do! parseToken TkDColon
        let! signature = parseSig
        let! def = parseFnDef
        let span = ident.Span ++ def.Span
        return { Ident = ident
                 Span = span
                 Sig = signature
                 Def = def }
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
        let! fnItem = parseFnItem
        return! mkItem fnItem.Span (ItemFn fnItem) }

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
