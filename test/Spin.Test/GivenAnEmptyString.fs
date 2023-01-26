namespace Spin.Test

open System

open Xunit
open FsUnit.Xunit

open Spin

module ``Given a string`` =

    let it = ""

    [<Fact>]
    let ``When trying to parse characters`` () =
        let struct (result, rest) =
            (Parser.many Parser.letter it)
            |> Result.toOption
            |> Option.get

        (List.length result) |> should equal 0


    [<Fact>]
    let ``When trying to parse at least one character`` () =
        let result =
            (Parser.atLeast1 Parser.letter it)
            |> Result.toOption

        result |> should equal None
