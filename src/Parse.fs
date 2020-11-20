module Parse

open Ast

type ParseError = ParseError

type ParseCtxt = { Src: list<char> }

type Parse<'a> = P of (ParseCtxt -> Result<'a * ParseCtxt, ParseError>)

let private bind f =
    function
    | Ok t -> f t
    | Error err -> Error err

let (>>=) x f = bind f x

module private Parsers =
    let ret t = P(fun pcx -> Ok(t, pcx))

    let bind f (P p) =
        P(fun pcx ->
                p pcx
                >>= (fun (t, pcx') -> let (P q) = f t in q pcx'))


    (* sequence (>>) in haskell *)
    (* let combine (P p) (P q) = P(fun src -> p src >>= fun (_, src') -> q src') *)

    let combine p q =
        p
        >>= fun _ -> q

    let zero () = P(fun _ -> Error ParseError)


type ParserBuilder() =
    member _x.Return t = Parsers.ret t
    member _x.ReturnFrom(p) = p
    member _x.Bind(t, f) = Parsers.bind f t
    member _x.Zero() = Parsers.zero ()
    member _x.Combine(p, q) = Parsers.combine p q
    member _x.Delay(f) = P(fun src -> let (P g) = f () in g src)

let private parse = ParserBuilder()


(* combinators *)


(* let integer = P(fun src -> ) *)

let private accept p =
    P(fun pctx -> if p pctx.Src.[0] then Ok(pctx.Src.[0], pctx) else Error ParseError)


let (<|>) (P p) (P q) =
    P(fun pctx ->
            match p pctx with
            | Ok (a, pctx') -> Ok(a, pctx')
            | Error _ -> q pctx)

let rec private many1 p =
    parse {
        let! x = p
        let! xs = many p
        return x :: xs
    }

and private many p = many1 p <|> Parsers.ret []

let rec private sequence =
    function
    | [] -> Parsers.ret []
    | p :: ps ->
        parse {
            let! x = p
            let! xs = sequence ps
            return x :: xs
        }

let private map f p =
    parse {
        let! x = p
        return f x
    }


(* parsing logic *)

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

let private parseChar (c: char) =
    P(fun pctx ->
            match pctx.Src with
            | [] -> Error ParseError
            | x :: xs ->
                let pctx' = { pctx with Src = xs }
                if c = x then Ok(c, pctx') else Error ParseError)

let charsToStr chars = System.String(List.toArray chars)

let private parseStr str: Parse<string> =
    str
    |> List.ofSeq
    |> List.map parseChar
    |> sequence
    |> map charsToStr

let private parseIdent: Parse<Ident> = failwith ""

let private parseTy: Parse<Type> = failwith ""

let private parseSig = parseTy


let private parseFunctionItem =
    parse {
        let! ident = parseIdent
        let! signature = parseSig
        return { Ident = ident; Sig = signature }
    }

let private parseItem = parseFunctionItem

let private parseProgram = many parseItem
