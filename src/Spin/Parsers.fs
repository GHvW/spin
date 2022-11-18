type ParseError = {
    Message: string
    CharacterPosition: int
}

type ParseResult<'In, 'Out> = Result<struct('Out * seq<'In>), ParseError>


type Parser<'In, 'Out> = seq<'In> -> ParseResult<'In, 'Out>


let item : Parser<'A, 'A> = 
    fun input ->
        if Seq.isEmpty input then 
            Error { Message = "sequence is empty"; CharacterPosition }
        else 
            Ok (Seq.head input, Seq.tail input)


let zero : Parser<'In, 'Out> = fun _ -> Error "Unable to Parse ... for now"


let succeed item : Parser<'In, 'Out> = 
    fun input -> Ok (item, input)


let map (func: 'b -> 'c) (parser: Parser<'a, 'b>): Parser<'a, 'c> =
    fun input ->
        input
        |> parser
        |> Result.map (fun struct (value, stream) -> (func value, stream))


let bind (func: 'b -> Parser<'a, 'c>) (parser: Parser<'a, 'b>): Parser<'a, 'c> =
    fun input ->
        input
        |> parser
        |> Result.bind (fun struct (value, stream) -> 
            (func value) stream)


type ParserBuilder() =
    member this.Zero () = zero
    member this.Bind (parser, func) = bind func parser
    member this.Return (item) = succeed item

let parse = ParserBuilder()


let satisfy (predicate: 'a -> bool): Parser<'a, 'a> =
    bind 
        (fun data ->
            if predicate data then
                succeed data
            else
                zero)
        item


let parseOr (second: Parser<'a, 'b>) (first: Parser<'a, 'b>): Parser<'a, 'b> =
    fun input ->
        match first input with
        | Error _ -> second input
        | ok -> ok


let character (it: char) : Parser<char, char> =
    satisfy (fun data -> it = data)


let rec str (it: string) : Parser<char, string> =
    match it with
    | "" -> succeed ""
    | _ -> parse {
        let! x = character (it.[0])
        let! y = str (it.[1..])
        return it
    }


let charDigit : Parser<char, char> =
    satisfy (fun data -> data >= '0' && data <= '9')


let upper : Parser<char, char> =
    satisfy (fun data -> data >= 'A' && data <= 'Z')


let lower : Parser<char, char> =
    satisfy (fun data -> data >= 'a' && data <= 'z')


let letter : Parser<char, char> =
    upper |> parseOr lower


let alphaNumeric : Parser<char, char> =
    charDigit |> parseOr letter


let rec many (parser: Parser<'a, 'b>): Parser<'a, list<'b>> = 
    parse {
        let! x = parser
        let! xs = many parser

        return x::xs
    } |> parseOr (succeed [])


// Many1
let rec manyAtLeast1 (parser : Parser<'a, 'b>): Parser<'a, list<'b>> =
    parse {
        let! x = parser
        let! xs = many parser

        return x::xs
    } 