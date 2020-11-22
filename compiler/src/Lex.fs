module Lex

open System
open State
open Span


type TokenKind =
    | TkIdent of Ident
    | TkInt of int
    | TkEq
    | TkLParen
    | TkRParen
    | TkRArrow
    | TkRFArrow
    | TkDColon
    | TkPlus
    | TkEOF
    | TkLet
    | TkSig
    | TkMinus
    | TkStar
    | TkSlash
    | TkBang
    | TkComma


let keywords =
    Map
        [ ("let", TkLet)
          ("sig", TkSig) ]

type Token =
    { Span: Span
      Kind: TokenKind }

type LexCtxt =
    { Src: list<char>
      Index: int }

    static member Default src =
        { Src = src
          Index = 0 }


type private Lex<'a> = State<LexCtxt, 'a>

let private lex = state

let private runLexer = runState


let private source =
    lex {
        let! lcx = get
        return lcx.Src }

#nowarn "40"

let mkTok span kind =
    lex {
        return { Span = span
                 Kind = kind }
    }

let index =
    lex {
        let! lcx = get
        return lcx.Index }

let setSrc src =
    lex {
        let! lcx = get
        do! put
                { lcx with
                      Src = src
                      Index = lcx.Index + (lcx.Src.Length - src.Length) }
    }

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

let rec withSpan f =
    lex {
        let! lo = index
        let! r = f
        let! hi = index
        let span =
            { Lo = lo
              Hi = hi }
        return (span, r)
    }

let rec skipLine =
    lex {
        match! source with
        | [] -> return ()
        | '\n' :: xs -> return! setSrc xs
        | _ :: xs ->
            do! setSrc xs
            return! skipLine
    }

let rec lexer =
    lex {
        match! source with
        | [] -> return []
        | '/' :: '/' :: _ ->
            do! skipLine
            return! lexer
        | ':' :: ':' :: xs -> return! addTok TkDColon xs
        | '-' :: '>' :: xs -> return! addTok TkRArrow xs
        | '=' :: '>' :: xs -> return! addTok TkRFArrow xs
        | '=' :: xs -> return! addTok TkEq xs
        | '!' :: xs -> return! addTok TkBang xs
        | '(' :: xs -> return! addTok TkLParen xs
        | ')' :: xs -> return! addTok TkRParen xs
        | '+' :: xs -> return! addTok TkPlus xs
        | '-' :: xs -> return! addTok TkMinus xs
        | '*' :: xs -> return! addTok TkStar xs
        | '/' :: xs -> return! addTok TkSlash xs
        | ',' :: xs -> return! addTok TkComma xs
        | (' '
        | '\t'
        | '\r'
        | '\n') :: xs -> return! withSrc xs
        | '_' :: _ -> return! lexIdent
        | x :: _ when Char.IsLetter x -> return! lexIdent
        | n :: _ when Char.IsDigit n -> return! lexInt
        | _ -> return failwith ""
    }

and lexIdent =
    lex {
        let! (span, ident) = withSpan lexIdentInner

        let kind =
            match keywords.TryFind ident with
            | Some kw -> kw
            | None ->
                (TkIdent
                    { Symbol = ident
                      Span = span })

        let! token = mkTok span kind
        let! tokens = lexer
        return token :: tokens
    }

and lexInt =
    lex {
        let! (span, str) = withSpan lexIntInner
        let i = int str

        let! token = mkTok span (TkInt i)
        let! tokens = lexer
        return token :: tokens
    }


and addTok kind src =
    lex {
        let! lo = index
        do! setSrc src
        let! hi = index
        let span =
            { Lo = lo
              Hi = hi }
        let! token = mkTok span kind
        let! tokens = lexer
        return token :: tokens
    }

(* lex `src` *)
and withSrc src =
    lex {
        do! setSrc src
        return! lexer
    }

let lexProgram src: list<Token> = runLexer lexer (LexCtxt.Default src) |> fst
