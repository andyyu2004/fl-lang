module Tests

open System.IO
open Format
open Compiler
open Xunit


let writeSourceToPath src =
    let path = Path.GetTempFileName()
    File.WriteAllText(path, src)
    path

let parse src expected =
    let ast = writeSourceToPath src |> runParsePhase

    let ast =
        match ast with
        | Ok ast -> ast
        | Error err -> failwith <| sprintf "%s" (show err)

    eprintfn "%s" (show ast)

    Assert.Equal(show ast, expected)


[<Fact>]
let parseFnSig() =
    let src = "f :: Int f = 5"
    let expected = "f :: Int\nf = 5"

    parse src expected

[<Fact>]
let parseArrowTyRightAssoc() =
    let src = "f :: Int -> Bool -> Int f = 5"
    let expected = "f :: (Int -> (Bool -> Int))\nf = 5"

    parse src expected

[<Fact>]
let parsedTyGroup() =
    let src = "f :: (Int -> Bool) -> Int f = 5"
    let expected = "f :: (((Int -> Bool)) -> Int)\nf = 5"
    parse src expected


[<Fact>]
let parseExprUnit() =
    let src = "f :: () f = ()"
    let expected = "f :: ()\nf = ()"
    parse src expected

[<Fact>]
let parseExprTuple() =
    let src = "f :: () f = (1,2,3)"
    let expected = "f :: ()\nf = (1,2,3)"
    parse src expected

[<Fact>]
let parseUnaryTuple() =
    // we know this will definitely not parse as a group due to the trailing comma
    // this is more a test that it succesfully parses as a tuple
    let src = "f :: () f = (1,)"
    let expected = "f :: ()\nf = (1)"
    parse src expected

[<Fact>]
let parseAssocLeftExpr() =
    let src = "f :: () f = 2 + 3 + 4"
    let expected = "f :: ()\nf = ((2 + 3) + 4)"
    parse src expected

[<Fact>]
let parseAssocLeftExprWithPrecedence() =
    let src = "f :: () f = 2 + 3 * 4"
    let expected = "f :: ()\nf = (2 + (3 * 4))"
    parse src expected

[<Fact>]
let parseUnary() =
    let src = "f :: () f = !-5"
    let expected = "f :: ()\nf = (!(-5))"
    parse src expected
