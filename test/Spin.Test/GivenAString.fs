namespace Spin.Test

open System

open Xunit
open FsUnit.Xunit

open Spin

module ``Given A String`` =

    let it = "Hello World!"

    [<Fact>]
    let ``When running success`` () =
        let result =
            Parser.succeed
                "succeed"
                { Input = (Memory(it.ToCharArray()))
                  Offset = 0 }
            |> Result.toOption
            |> Option.get

        result.Item |> should equal "succeed"
        result.CharsConsumed |> should equal 0

    [<Fact>]
    let ``When running zero`` () =
        let result =
            (Parser.zero
                { Input = (Memory(it.ToCharArray()))
                  Offset = 0 })
            |> Result.toOption

        result |> should equal None

    [<Fact>]
    let ``When parsing a single letter`` () =
        let result =
            Parser.letter
                { Input = Memory(it.ToCharArray())
                  Offset = 0 }
            |> Result.toOption
            |> Option.get

        result.Item |> should equal 'H'

        result.CharsConsumed |> should equal 1


    [<Fact>]
    let ``When parsing a product`` () =
        let result =
            Parser.product
                (Parser.character 'e')
                (Parser.character 'H')
                { Input = Memory(it.ToCharArray())
                  Offset = 0 }
            |> Result.toOption
            |> Option.get

        let (first, second) = result.Item
        first |> should equal 'H'
        second |> should equal 'e'

        result.CharsConsumed |> should equal 2


    [<Fact>]
    let ``When applying multiple parsers`` () =
        let newParser =
            (Parser.succeed (fun x y z -> $"{x} + {y} + {z}")
             |> Parser.apply (Parser.character 'H')
             |> Parser.apply (Parser.character 'e')
             |> Parser.apply (Parser.character 'l'))

        let result =
            newParser
                { Input = Memory(it.ToCharArray())
                  Offset = 0 }
            |> Result.toOption
            |> Option.get

        result.Item |> should equal "H + e + l"

        result.CharsConsumed |> should equal 3


    [<Fact>]
    let ``When skipping characters between other parsers`` () =
        let newParser =
            (Parser.succeed (fun x z -> $"{x} + {z}")
             |> Parser.apply (Parser.character 'H')
             |> Parser.skip (Parser.character 'e')
             |> Parser.apply (Parser.character 'l'))

        let result =
            newParser
                { Input = Memory(it.ToCharArray())
                  Offset = 0 }
            |> Result.toOption
            |> Option.get

        result.Item |> should equal "H + l"

        result.CharsConsumed |> should equal 3


    [<Fact>]
    let ``When parsing a single word`` () =
        let struct (result, rest) = Parser.word it |> Result.toOption |> Option.get

        result |> should equal [ 'H'; 'e'; 'l'; 'l'; 'o' ]
        (String.Concat(rest)) |> should equal " World!"


    module ``That contains digits`` =

        let newIt = "3llo W0rld"

        [<Fact>]
        let ``When parsing a digit`` () =
            let struct (result, rest) = Parser.digit newIt |> Result.toOption |> Option.get

            result |> should equal '3'
            (String.Concat(rest)) |> should equal "llo W0rld"


        let ``When parsing a number in the 100s`` () =
            let struct (result, rest) = Parser.natural "150s" |> Result.toOption |> Option.get

            result |> should equal 150
            (String.Concat(rest)) |> should equal "s"
