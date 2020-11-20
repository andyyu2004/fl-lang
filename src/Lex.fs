module Lex

open System
open State

type TokenKind =
    | TkInt of int
    | TkLParen
    | TkRParen
    | TkDColon
    | TkPlus
    | TkMinus
    | TkStar


type Token =
    { Kind: TokenKind }

type LexCtxt =
    { Src: list<char> }
    static member Default src = { Src = src }


type private Lex<'a> = State<LexCtxt, 'a>

let private lex = state

let private runLexer = runState


let private source =
    lex {
        let! lcx = get
        return lcx.Src }

#nowarn "40"

let mkTok kind = lex { return { Kind = kind } }

let setSrc src =
    lex {

        let! lcx = get
        do! put { lcx with Src = src } }

(* returns the characters that make up an integer *)
let rec lexIntInner =
    lex {
        match! source with
        | n :: src when Char.IsDigit n ->
            do! setSrc src
            let! rest = lexIntInner
            return n :: rest
        | _ -> return []
    }


let rec lexer =
    lex {
        match! source with
        | [] -> return []
        | ':' :: ':' :: xs -> return! addTok TkDColon xs
        | '(' :: xs -> return! addTok TkLParen xs
        | ')' :: xs -> return! addTok TkRParen xs
        | '+' :: xs -> return! addTok TkPlus xs
        | '-' :: xs -> return! addTok TkMinus xs
        | '*' :: xs -> return! addTok TkStar xs
        | n :: _ when Char.IsDigit n -> return! lexInt
        | (' '
        | '\t'
        | '\n') :: xs -> return! withSrc xs
        | _ -> failwith ""
    }

and lexInt =
    lex {
        let! nums = lexIntInner
        let i = new string(Array.ofList nums) |> int
        let! token = mkTok (TkInt i)
        let! tokens = lexer
        return token :: tokens
    }


and addTok kind src =
    lex {
        let! token = mkTok kind
        let! tokens = withSrc src
        return token :: tokens }

(* lex `src` *)
and withSrc src =
    lex {
        do! setSrc src
        return! lexer
    }

let lexProgram src = runLexer lexer (LexCtxt.Default src) |> snd
