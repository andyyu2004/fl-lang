module Lex

open System
open State

type Span =
    { Lo: int
      Hi: int }

type Ident =
    { Symbol: string
      Span: Span }

type TokenKind =
    | TkIdent of Ident
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

let rec lexWhileInner p =
    lex {
        match! source with
        | x :: src when p x ->
            do! setSrc src
            let! xs = lexWhileInner p
            return x :: xs
        | _ -> return []
    }

let rec lexWhile p =
    lex {
        let! chars = lexWhileInner p
        return new string(Array.ofList chars) }


(* returns the characters that make up an integer *)
let rec lexIntInner = lexWhile Char.IsDigit

let rec lexIdentInner = lexWhile Char.IsLetter


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
        | (' '
        | '\t'
        | '\n') :: xs -> return! withSrc xs
        | '_' :: _ -> return! lexIdent
        | x :: _ when Char.IsLetter x -> return! lexIdent
        | n :: _ when Char.IsDigit n -> return! lexInt
        | _ -> failwith ""
    }

and lexIdent =
    lex {
        let! ident = lexIdentInner
        let! token = mkTok
                         (TkIdent
                             { Symbol = ident
                               Span =
                                   { Lo = 0
                                     Hi = 0 } })
        let! tokens = lexer
        return token :: tokens
    }

and lexInt =
    lex {
        let! str = lexIntInner
        let i = int str
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

let lexProgram src: list<Token> = runLexer lexer (LexCtxt.Default src) |> fst
