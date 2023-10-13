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
                (Location.init it)
            |> Result.toOption
            |> Option.get

        result.Item |> should equal "succeed"
        result.CharsConsumed |> should equal 0

    [<Fact>]
    let ``When running zero`` () =
        let result =
            (Parser.zero
                ( Location.init it))
            |> Result.toOption

        result |> should equal None

    [<Fact>]
    let ``When parsing a single letter`` () =
        let result =
            Parser.letter (Location.init it)
            |> Result.toOption
            |> Option.get

        result.Item |> should equal 'H'

        result.CharsConsumed |> should equal 1


    [<Fact>]
    let ``When parsing starting a's out of aaaabbbb`` () =
        let result =
            Parser.many (Parser.character 'a') (Location.init "aaaabbbb")
            |> Result.toOption
            |> Option.get

        result.Item |> should equal ['a'; 'a'; 'a'; 'a']
        result.CharsConsumed |> should equal 4

    [<Fact>]
    let ``When parsing starting c's out of aaaabbbb`` () =
        let result =
            Parser.many (Parser.character 'c') (Location.init "aaaabbbb")
            |> Result.toOption
            |> Option.get

        result.Item |> should be Empty
        result.CharsConsumed |> should equal 0


    [<Fact>]
    let ``When parsing items separated by commas`` () =
        let result =
            Parser.separatedBy (Parser.character ',') (Parser.character 'a') (Location.init "a,a,a,abb")
            |> Result.toOption
            |> Option.get

        result.Item |> should equal ['a'; 'a'; 'a'; 'a']
        result.CharsConsumed |> should equal 7


    [<Fact>]
    let ``When parsing items separated by commas but there are none`` () =
        let result =
            Parser.separatedBy (Parser.character ',') (Parser.character 'c') (Location.init "a,a,a,abb")
            |> Result.toOption
            |> Option.get

        result.Item |> should be Empty
        result.CharsConsumed |> should equal 0

    [<Fact>]
    let ``When parsing at least 1 item separated by commas`` () =
        let result =
            Parser.atLeast1SeparatedBy (Parser.character ',') (Parser.character 'a') (Location.init "abb")
            |> Result.toOption
            |> Option.get

        result.Item |> should equal ['a']
        result.CharsConsumed |> should equal 1


    [<Fact>]
    let ``When parsing at least 1 item separated by +'s and there are multiple`` () =
        let result =
            Parser.atLeast1SeparatedBy (Parser.character '+') (Parser.character 'a') (Location.init "a+a+abb")
            |> Result.toOption
            |> Option.get

        result.Item |> should equal ['a'; 'a'; 'a']
        result.CharsConsumed |> should equal 5


    [<Fact>]
    let ``When parsing at least 1 item separated by commas but the input is bad`` () =
        let result =
            Parser.atLeast1SeparatedBy (Parser.character ',') (Parser.character 'a') (Location.init "c,a,a,abb")
            |> Result.toOption

        result |> should equal None


    [<Fact>]
    let ``When parsing a product`` () =
        let result =
            Parser.product
                (Parser.character 'e')
                (Parser.character 'H')
                (Location.init it)
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
            newParser (Location.init it)
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
            newParser (Location.init it)
            |> Result.toOption
            |> Option.get

        result.Item |> should equal "H + l"

        result.CharsConsumed |> should equal 3


    [<Fact>]
    let ``When parsing a single word`` () =
        let result = 
            Parser.word (Location.init it)
            |> Result.toOption 
            |> Option.get

        result.Item |> should equal [ 'H'; 'e'; 'l'; 'l'; 'o' ]
        result.CharsConsumed |> should equal 5


    module ``That contains digits`` =

        let newIt = "3llo W0rld"

        [<Fact>]
        let ``When parsing a digit`` () =
            let result = 
                Parser.digit (Location.init newIt)
                |> Result.toOption 
                |> Option.get

            result.Item |> should equal '3'
            result.CharsConsumed |> should equal 1


        [<Fact>]
        let ``When parsing a number in the 100s`` () =
            let result = 
                Parser.natural (Location.init "150s")
                |> Result.toOption 
                |> Option.get

            result.Item |> should equal 150
            result.CharsConsumed |> should equal 3

        [<Fact>]
        let ``when parsing letters only`` () =
            let result = 
                Parser.many (Parser.letter) (Location.init newIt)
                |> Result.toOption
                |> Option.get

            result.Item |> should be Empty
            result.CharsConsumed |> should equal 0
