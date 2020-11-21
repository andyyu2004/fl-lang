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


let (<|>) (P p) (P q) =
    P(fun pctx ->
            match p pctx with
            | (Ok a, pctx') -> (Ok a, pctx')
            | (Error _, pctx') -> q pctx')

let rec private many1 p: Parse<list<'a>> =
    parse {
        let! x = p
        let! xs = many p
        return x :: xs }

and private many p: Parse<list<'a>> = many1 p <|> Parsers.ret []

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

let private parsePathSegment: Parse<PathSegment> =
    parse {
        let! ident = expectIdent
        return { Ident = ident } }

let private parsePathTy =
    parse {
        let! segment = parsePathSegment
        let path = { Segments = [ segment ] }
        return AstTyPath path
    }

#nowarn "40"

let rec private parseTy: Parse<Type> =
    parse {
        // use the <|> combinator to parse all the potential different types
        let! ty = parsePathTy
        match! accept TkRArrow with
        | None -> return ty
        | Some _ ->
            let! rty = parseTy
            return AstTyFn(ty, rty)
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

let rec private parseExpr: Parse<Expr> = parseTerm

and parseTerm =
    parse {
        let! left = parseFactor
        return! parseTerm' left }

and parseTerm' l =
    parse {
        let! token = acceptOneOf [ TkPlus; TkMinus ]
        if Option.isSome token then
            let binop = token.Value.Kind |> BinOp.FromToken
            let! r = parseFactor
            let span = l.Span ++ r.Span
            let kind = ExprBin(binop, l, r)
            let! expr = mkExpr span kind
            return! parseTerm' expr
        else
            return l
    }

and parseFactor: Parse<Expr> =
    parse {
        let! left = parsePrimary
        return! parseFactor' left }

and parseFactor' l =
    parse {
        let! token = acceptOneOf [ TkStar; TkSlash ]
        if Option.isSome token then
            let binop = token.Value.Kind |> BinOp.FromToken
            let! r = parsePrimary
            let span = l.Span ++ r.Span
            let kind = ExprBin(binop, l, r)
            let! expr = mkExpr span kind
            return! parseFactor' expr
        else
            return l
    }

and parseGroupExpr =
    parse {
        let! _ = expect TkLParen
        let! expr = parseExpr
        let! _ = expect TkRParen
        return expr }

and parsePrimary: Parse<Expr> = parseLiteralExpr <|> parseGroupExpr

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
        let! _ = expect TkEq
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
        let! _ = expect TkDColon
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
