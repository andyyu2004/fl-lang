module Parse

open Ast

type ParseError = ParseError

type Parse<'a> = P of (list<char> -> Result<'a * list<char>, ParseError>)


let bind f =
    function
    | Ok t -> f t
    | Error err -> Error err

let (>>=) x f = bind f x

module Parsers =
    let bind f (P p) =
        P(fun src ->
                p src
                >>= (fun (t, src') -> let (P q) = f t in q src'))


    (* sequence (>>) in haskell *)
    (* let combine (P p) (P q) = P(fun src -> p src >>= fun (_, src') -> q src') *)
    let combine p q = p >>= fun _ -> q

    let zero() = P(fun _ -> Error ParseError)


type ParserBuilder() =
    member _x.Return t = P(fun src -> Ok(t, src))
    member _x.ReturnFrom(p) = p
    member _x.Bind(t, f) = Parsers.bind f t
    member _x.Zero() = Parsers.zero()
    member _x.Combine(p, q) = Parsers.combine p q
    member _x.Delay(f) =
        P(fun src -> let (P g) = f() in g src)

let parse = ParserBuilder()

let parseExpr src = src

(* let integer = P(fun src -> ) *)
let accept p =
    P(fun src -> if p src.[0] then Ok(src.[0], src.[1..]) else Error ParseError)


let rec many1 p =
    parse {
        let! x = p
        let! xs = many p
        return x :: xs }

and many p =
    failwith
        (* parse { *)
        (*     return! many1 p *)
        (*     return [] *)
        (* } *) ""
