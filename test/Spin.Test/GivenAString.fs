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
                { Input = it.AsMemory(); Offset = 0 }
            |> Result.toOption
            |> Option.get

        result.Item |> should equal "succeed"
        result.CharsConsumed |> should equal 0

    [<Fact>]
    let ``When running zero`` () =
        let result =
            (Parser.zero
                { Input = it.AsMemory(); Offset = 0 })
            |> Result.toOption

        result |> should equal None

    [<Fact>]
    let ``When parsing a single letter`` () =
        let result =
            Parser.letter
                { Input = it.AsMemory();
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
                { Input = it.AsMemory();
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
                { Input = it.AsMemory();
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
                { Input = it.AsMemory();
                  Offset = 0 }
            |> Result.toOption
            |> Option.get

        result.Item |> should equal "H + l"

        result.CharsConsumed |> should equal 3


    [<Fact>]
    let ``When parsing a single word`` () =
        let result = 
            Parser.word { Input = it.AsMemory(); Offset = 0 } 
            |> Result.toOption 
            |> Option.get

        result.Item |> should equal [ 'H'; 'e'; 'l'; 'l'; 'o' ]
        result.CharsConsumed |> should equal 5


    module ``That contains digits`` =

        let newIt = "3llo W0rld"

        [<Fact>]
        let ``When parsing a digit`` () =
            let result = 
                Parser.digit { Input = newIt.AsMemory(); Offset = 0 } 
                |> Result.toOption 
                |> Option.get

            result.Item |> should equal '3'
            result.CharsConsumed |> should equal 1


        let ``When parsing a number in the 100s`` () =
            let result = 
                Parser.natural { Input = "150s".AsMemory(); Offset = 0 } 
                |> Result.toOption 
                |> Option.get

            result.Item |> should equal 150
            result.CharsConsumed |> should equal 3
