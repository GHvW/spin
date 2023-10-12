namespace Spin.Test

open System

open Xunit
open FsUnit.Xunit

open Spin.Parser
open Spin

module ``Given an empty string`` =

    let it = ""

    [<Fact>]
    let ``When trying to parse characters`` () =
        let result = 
            many letter (Location.init "")
            |> Result.toOption 
            |> Option.get

        result.Item |> should be Empty
        result.CharsConsumed |> should equal 0


    [<Fact>]
    let ``When trying to parse at least one character`` () =
        let result = 
            atLeast1 letter (Location.init it)
            |> Result.toOption

        result |> should equal None
