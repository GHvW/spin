namespace Spin.Test

open System

open Xunit
open FsUnit.Xunit

open Spin

module ``Given A String`` =

    let it = "Hello World!"

    [<Fact>]
    let ``When running success`` () =
        let struct (result, rest) =
            Parser.succeed "succeed" it
            |> Result.toOption
            |> Option.get

        result |> should equal "succeed"
        rest |> should equal "Hello World!"

    [<Fact>]
    let ``When running zero`` () =
        let result = Parser.zero it |> Result.toOption

        result |> should equal None

    [<Fact>]
    let ``When parsing a single letter`` () =
        let struct (result, rest) =
            Parser.letter it |> Result.toOption |> Option.get

        result |> should equal 'H'

        (String.Concat(rest))
        |> should equal "ello World!"


    [<Fact>]
    let ``When parsing a product`` () =
        let struct ((result1, result2), rest) =
            Parser.product (Parser.character 'e') (Parser.character 'H') it
            |> Result.toOption
            |> Option.get

        result1 |> should equal 'H'
        result2 |> should equal 'e'

        (String.Concat(rest)) |> should equal "llo World!"


    [<Fact>]
    let ``When applying multiple parsers`` () =
        let newParser =
            (Parser.succeed (fun x y z -> $"{x} + {y} + {z}")
             |> Parser.apply (Parser.character 'H')
             |> Parser.apply (Parser.character 'e')
             |> Parser.apply (Parser.character 'l'))

        let struct (result, rest) =
            newParser it |> Result.toOption |> Option.get

        result |> should equal "H + e + l"

        (String.Concat(rest)) |> should equal "lo World!"


    [<Fact>]
    let ``When skipping characters between other parsers`` () =
        let newParser =
            (Parser.succeed (fun x z -> $"{x} + {z}")
             |> Parser.apply (Parser.character 'H')
             |> Parser.skip (Parser.character 'e')
             |> Parser.apply (Parser.character 'l'))

        let struct (result, rest) =
            newParser it |> Result.toOption |> Option.get

        result |> should equal "H + l"

        (String.Concat(rest)) |> should equal "lo World!"


    [<Fact>]
    let ``When parsing a single word`` () =
        let struct (result, rest) =
            Parser.word it |> Result.toOption |> Option.get

        result |> should equal [ 'H'; 'e'; 'l'; 'l'; 'o' ]
        (String.Concat(rest)) |> should equal " World!"


    module ``That contains digits`` =

        let newIt = "3llo W0rld"

        [<Fact>]
        let ``When parsing a digit`` () =
            let struct (result, rest) =
                Parser.digit newIt
                |> Result.toOption
                |> Option.get

            result |> should equal '3'
            (String.Concat(rest)) |> should equal "llo W0rld"


        let ``When parsing a number in the 100s`` () =
            let struct (result, rest) =
                Parser.natural "150s"
                |> Result.toOption
                |> Option.get

            result |> should equal 150
            (String.Concat(rest)) |> should equal "s"
