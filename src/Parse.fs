module Parse

/// parser monad
type Parse<'T> = P of (list<char> -> seq<'T * list<char>>)

module Parsers =
    let bind f (P p) =
        P(fun src ->
                seq {
                    for (t, src') in p src do
                        let (P q) = f t
                        yield! q src'
                })

    let combine (P p) (P q) =
        P(fun src ->
                Seq.concat
                    [ p src
                      q src ])

    let zero() = P(fun _ -> Seq.empty)

type ParserBuilder() =
    member _x.Return t = P(fun src -> seq [ t, src ])
    member _x.ReturnFrom(p) = p
    member _x.Bind(t, f) = Parsers.bind f t
    member _x.Zero() = Parsers.zero()
    member _x.Combine(p, q) = Parsers.combine p q
    member _x.Delay(f) =
        P(fun src -> let (P g) = f() in g src)

let parse = ParserBuilder()

/// combinators

let rec many1 p =
    parse {
        let! x = p
        let! xs = many p
        return x :: xs }

and many p =
    parse {
        return! many1 p
        return []
    }
