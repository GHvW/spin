module Spin.Parser

open System

type ParseError =
    { Message: string
      CharacterPosition: int }

type ParseSuccess = { CharacterPosition: int }

type ParseResult<'Out> = Result<struct ('Out * seq<char>), ParseError>


type Parser<'Out> = seq<char> -> ParseResult<'Out>


let item: Parser<char> =
    fun input ->
        if Seq.isEmpty input then
            Error
                { Message = "sequence is empty"
                  CharacterPosition = 0 }
        else
            Ok(Seq.head input, Seq.tail input)


let zero: Parser<'Out> =
    fun _ ->
        Error
            { Message = "Unable to Parse ... for now"
              CharacterPosition = 0 }


let succeed item : Parser<'Out> = fun input -> Ok(item, input)


let apply (valParser: Parser<'A>) (fnParser: Parser<'A -> 'B>) : Parser<'B> =
    fun input ->
        fnParser input
        |> Result.bind (fun struct (fn, next) ->
            valParser next
            |> Result.map (fun struct (it, rest) -> struct (fn it, rest)))


let product (second: Parser<'B>) (first: Parser<'A>) : Parser<'A * 'B> =
    fun input ->
        first input
        |> Result.bind (fun struct (item1, rest) ->
            second rest
            |> Result.map (fun struct (item2, rest2) -> struct ((item1, item2), rest2)))


let map (func: 'A -> 'B) (parser: Parser<'A>) : Parser<'B> =
    fun input ->
        input
        |> parser
        |> Result.map (fun struct (value, stream) -> (func value, stream))


let map2 (func: 'A -> 'B -> 'C) (other: Parser<'B>) (parser: Parser<'A>) : Parser<'C> =
    succeed func |> apply parser |> apply other


let map3 (func: 'A -> 'B -> 'C -> 'D) (mid: Parser<'B>) (last: Parser<'C>) (first: Parser<'A>) : Parser<'D> =
    succeed func
    |> apply first
    |> apply mid
    |> apply last


let bind (func: 'A -> Parser<'B>) (parser: Parser<'A>) : Parser<'B> =
    fun input ->
        input
        |> parser
        |> Result.bind (fun struct (value, stream) -> (func value) stream)


type ParserBuilder() =
    member this.Zero() = zero
    member this.Bind(parser: Parser<'A>, func: 'A -> Parser<'B>) : Parser<'B> = bind func parser
    member this.Return(item: 'A) : Parser<'A> = succeed item

let parser = ParserBuilder()


let satisfy (predicate: char -> bool) : Parser<char> =
    bind (fun c -> if predicate c then succeed c else zero) item


let orElse (second: Parser<'A>) (first: Parser<'A>) : Parser<'A> =
    fun input ->
        match first input with
        | Error _ -> second input
        | ok -> ok


let character (it: char) : Parser<char> = satisfy (fun data -> it = data)


let rec str (it: string) : Parser<string> =
    match it with
    | "" -> succeed ""
    | _ ->
        parser {
            let! x = character (it.[0])
            let! y = str (it.[1..])
            return it
        }


let digit: Parser<char> = satisfy Char.IsDigit


let upper: Parser<char> = satisfy Char.IsUpper


let lower: Parser<char> = satisfy Char.IsLower


let letter: Parser<char> = upper |> orElse lower


let alphaNumeric: Parser<char> = digit |> orElse letter


let whitespace: Parser<char> = satisfy Char.IsWhiteSpace


let rec many (parse: Parser<'A>) : Parser<list<'A>> =
    parser {
        let! x = parse
        let! xs = many parse

        return x :: xs
    }
    |> orElse (succeed [])


// Many1
let rec atLeast1 (parse: Parser<'A>) : Parser<list<'A>> =
    parser {
        let! x = parse
        let! xs = many parse

        return x :: xs
    }

let word: Parser<list<char>> = fun input -> input |> (many letter)


let token (parse: Parser<'A>) : Parser<'A> =
    parser {
        let! it = parse
        let! _ = many whitespace
        return it
    }

// next - '0' gets the number between 9 and 0 inclusive without needing to parse the int
// 10 * current + next shifts the digit on each iteration so that 8 + 7 + 1 + 5 = 8715 instead of 21
let private scale (current: int) (next: char) : int =
    (10 * current) + ((int next) - (int '0'))


let natural: Parser<int> =
    atLeast1 digit |> map (List.fold scale 0)
