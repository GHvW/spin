namespace Spin.Test

open System

open Xunit
open FsUnit.Xunit

open Spin.Parser

module ``Given a string`` =

    let it = ""

    [<Fact>]
    let ``When trying to parse characters`` () =
        let result = run (many (letter)) (Memory(it.ToCharArray()))

        (List.length result) |> should equal 0


    [<Fact>]
    let ``When trying to parse at least one character`` () =
        let result = run (atLeast1 letter) (Memory(it.ToCharArray()))

        result |> should equal None
