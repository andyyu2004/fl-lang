module Parse

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
                | Error err -> (Error err, pcx')
                | Ok t -> runParser (f t) pcx')

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


(* combinators *)


(* let integer = P(fun src -> ) *)

let private accept p =
    P(fun pctx -> if p pctx.Tokens.[0] then (Ok pctx.Tokens.[0], pctx) else (Error ParseError, pctx))


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

let private nextId =
    parse {
        let! pcx = get
        let idx = pcx.IdCounter
        do! put { pcx with IdCounter = 1 + idx }
        return idx
    }


let private next: Parse<Token> =
    parse {
        let! pcx = get
        match pcx.Tokens with
        | [] -> return! error ParseError
        | t :: ts ->
            do! put { pcx with Tokens = ts }
            return t
    }

let private acceptIdent: Parse<Option<Ident>> =
    parse {
        let! token = next
        match token.Kind with
        | TkIdent ident -> return Some ident
        | _ -> return None
    }

let private expectIdent = map Option.get acceptIdent
let private parseTy: Parse<Type> = failwith ""
let private parseSig = parseTy

let private parseFunctionItem =
    parse {
        let! ident = expectIdent
        let! signature = parseSig
        return { Ident = ident
                 Sig = signature }
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
let private parseItem = parseFunctionItem

let private parseProgramInner = many parseItem

let parseProgram tokens =
    runParser parseProgramInner
        { Tokens = tokens
          IdCounter = 0 }
