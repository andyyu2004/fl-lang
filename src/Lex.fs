module Lex

open State

type TokenKind =
    | TkLParen
    | TkRParen
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

let rec lexer =
    lex {
        match! source with
        | [] -> return []
        | '(' :: xs -> return! addTok TkLParen xs
        | ')' :: xs -> return! addTok TkRParen xs
        | '+' :: xs -> return! addTok TkPlus xs
        | '-' :: xs -> return! addTok TkMinus xs
        | '*' :: xs -> return! addTok TkStar xs
        | (' '
        | '\n') :: xs -> return! withSrc xs
        | _ -> failwith ""
    }

and addTok kind src =
    lex {
        let token = { Kind = kind }
        let! tokens = withSrc src
        return token :: tokens
    }

(* lex `src` *)
and withSrc src =
    lex {
        let! lcx = get
        do! put { lcx with Src = src }
        return! lexer
    }

let lexProgram src = runLexer lexer (LexCtxt.Default src) |> snd
