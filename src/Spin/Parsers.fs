module Spin.Parsers

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


let apply (fnParser: Parser<'A -> 'B>) (valParser: Parser<'A>) : Parser<'B> =
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


let bind (func: 'A -> Parser<'B>) (parser: Parser<'A>) : Parser<'B> =
    fun input ->
        input
        |> parser
        |> Result.bind (fun struct (value, stream) -> (func value) stream)


type ParserBuilder() =
    member this.Zero() = zero
    member this.Bind(parser: Parser<'A>, func: 'A -> Parser<'B>) : Parser<'B> = bind func parser
    member this.MergeSources(parser1: Parser<'A>, parser2: Parser<'B>) : Parser<'A * 'B> = product parser2 parser1
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
            and! y = str (it.[1..])
            return it
        }


let charDigit: Parser<char> =
    satisfy (fun data -> data >= '0' && data <= '9')


let upper: Parser<char> =
    satisfy (fun data -> data >= 'A' && data <= 'Z')


let lower: Parser<char> =
    satisfy (fun data -> data >= 'a' && data <= 'z')


let letter: Parser<char> = upper |> orElse lower


let alphaNumeric: Parser<char> = charDigit |> orElse letter


let rec many (parseA: Parser<'A>) : Parser<list<'A>> =
    parser {
        let! x = parseA
        and! xs = many parseA

        return x :: xs
    }
    |> orElse (succeed [])


// Many1
let rec atLeast1 (parseA: Parser<'A>) : Parser<list<'A>> =
    parser {
        let! x = parseA
        and! xs = many parseA

        return x :: xs
    }
