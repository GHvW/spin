namespace Spin.Test

open System

open Xunit
open FsUnit.Xunit

open Spin.Parser

module ``Given a string`` =

    let it = ""

    [<Fact>]
    let ``When trying to parse characters`` () =
        let result  =
            (many (letter it))
            |> Result.toOption
            |> Option.get

        (List.length result) |> should equal 0


    [<Fact>]
    let ``When trying to parse at least one character`` () =
        let result =
            (atLeast1 letter it)
            |> Result.toOption

        result |> should equal None
