module Tests

open System
open System.IO
open Format
open Compiler
open Xunit


let writeSourceToPath src =
    let path = Path.GetTempFileName()
    File.WriteAllText(path, src)
    path

let parse (src: string) (expected: string) =
    let ast = writeSourceToPath src |> runParsePhase

    let ast =
        match ast with
        | Ok ast -> ast
        | Error err -> failwith <| sprintf "%s" (show err)

    Assert.Equal((show ast).Trim(), expected.Trim())


[<Fact>]
let parseFnSig() =
    let src = "sig f :: Int"
    let expected = "sig f :: Int"

    parse src expected

[<Fact>]
let parseArrowTyRightAssoc() =
    let src = "sig f :: Int -> Bool -> Int"
    let expected = "sig f :: (Int -> (Bool -> Int))"

    parse src expected

[<Fact>]
let parsedTyGroup() =
    let src = "sig f :: (Int -> Bool) -> Int"
    let expected = "sig f :: (((Int -> Bool)) -> Int)"
    parse src expected


[<Fact>]
let parseExprUnit() =
    let src = "let f = ()"
    let expected = "let f = ()"
    parse src expected

[<Fact>]
let parseExprTuple() =
    let src = "let f = (1, 2, 3)"
    let expected = "let f = (1,2,3)"
    parse src expected

[<Fact>]
let parseUnaryTuple() =
    // we know this will definitely not parse as a group due to the trailing comma
    // this is more a test that it succesfully parses as a tuple
    let src = "let f = (1,)"
    let expected = "let f = (1)"
    parse src expected

[<Fact>]
let parseAssocLeftExpr() =
    let src = "let f = 2 + 3 + 4"
    let expected = "let f = ((2 + 3) + 4)"
    parse src expected

[<Fact>]
let parseAssocLeftExprWithPrecedence() =
    let src = "let f = 2 + 3 * 4"
    let expected = "let f = (2 + (3 * 4))"
    parse src expected

[<Fact>]
let parseUnary() =
    let src = "let f = !-5"
    let expected = "let f = (!(-5))"
    parse src expected


[<Fact>]
let parsePatBind() =
    let src = "let f x = x"
    let expected = "let f x = x"
    parse src expected

[<Fact>]
let parseMultiplePatBind() =
    let src = "let f x y = x"
    let expected = "let f x y = x"
    parse src expected

[<Fact>]
let parsePatTuple() =
    let src = "let f (x, y) z = z"
    let expected = "let f (x,y) z = z"
    parse src expected
