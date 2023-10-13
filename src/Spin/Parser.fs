module Spin.Parser

open System


[<Struct>]
type ParseError =
    {
      // Stack: list<Location * string>;
      Stack: list<int * string> // (offset, error messag)
      IsCommitted: bool }


[<Struct>]
type ParseSuccess<'A> = { Item: 'A; CharsConsumed: int }


type ParseResult<'Out> = Result<ParseSuccess<'Out>, ParseError>


let inline uncommit (result: ParseResult<'A>) : ParseResult<'A> =
    match result with
    | Error { Stack = s; IsCommitted = true } -> Error { Stack = s; IsCommitted = true }
    | _ -> result

let inline advanceSuccess (n: int) (result: ParseResult<'A>) : ParseResult<'A> =
    match result with
    | Ok success -> Ok { success with CharsConsumed = success.CharsConsumed + n }
    | _ -> result


type Parser<'Out> = Location -> ParseResult<'Out>


// TODO - error reporting
let run (parser: Parser<'A>) (input: ReadOnlyMemory<char>) : 'A =
    match parser { Input = input; Offset = 0 } with
    | Ok { Item = it; CharsConsumed = _ } -> it
    | Error _ -> raise (Exception $"Error parsing {input}")


// TODO - error reporting
let zero: Parser<'Out> =
    fun input ->
        Error
            { Stack = [ (input.Offset, "zero error") ]
              IsCommitted = true }


let succeed it : Parser<'Out> =
    fun input -> 
        Ok { Item = it; CharsConsumed = 0 }


let satisfy (predicate: char -> bool) : Parser<char> =
    fun input ->
        if input.Input.IsEmpty then
            Error
                { Stack = [ (input.Offset, "sequence is empty") ]
                  IsCommitted = true }
        else if input.Input.Length <= input.Offset then
            Error { Stack = [ (input.Offset, "Offset is our of range")]; IsCommitted = true }
        else
            let charToCheck = input.Input.Span[input.Offset]
            if predicate charToCheck then
                Ok { Item = charToCheck; CharsConsumed = 1 }
            else
                Error
                    { Stack = [ (input.Offset, $"{input.Input.Span[input.Offset]} did not satisfy condition") ];
                    IsCommitted = true }


let apply (valParser: Parser<'A>) (fnParser: Parser<'A -> 'B>) : Parser<'B> =
    fun input ->
        fnParser input
        |> Result.bind (fun { Item = fn; CharsConsumed = next } ->
            valParser (input |> Location.advanceBy next)
            |> Result.map (fun { Item = it; CharsConsumed = rest } ->
                { Item = fn it
                  CharsConsumed = input.Offset + next + rest }))


let product (second: Parser<'B>) (first: Parser<'A>) : Parser<'A * 'B> =
    fun input ->
        first input
        |> Result.bind (fun { Item = item1; CharsConsumed = rest } ->
            second (input |> Location.advanceBy rest)
            |> Result.map (fun { Item = item2; CharsConsumed = rest2 } ->
                { Item = (item1, item2)
                  CharsConsumed = input.Offset + rest + rest2 }))


let map (func: 'A -> 'B) (parser: Parser<'A>) : Parser<'B> =
    fun input ->
        input
        |> parser
        |> Result.map (fun { Item = value; CharsConsumed = rest } ->
            { Item = func value
              CharsConsumed = input.Offset + rest })


let map2 (func: 'A -> 'B -> 'C) (other: Parser<'B>) (parser: Parser<'A>) : Parser<'C> =
    parser |> map func |> apply other


let map3 (func: 'A -> 'B -> 'C -> 'D) (mid: Parser<'B>) (last: Parser<'C>) (first: Parser<'A>) : Parser<'D> =
    first |> map func |> apply mid |> apply last


let bind (func: 'A -> Parser<'B>) (parser: Parser<'A>) : Parser<'B> =
    fun input ->
        input
        |> parser
        |> Result.bind (fun { Item = value; CharsConsumed = rest } -> 
             match (func value) (input |> Location.advanceBy rest) with
             | Ok it -> Ok { it with CharsConsumed = it.CharsConsumed + rest }
             | Error err -> Error { err with IsCommitted = err.IsCommitted || (rest <> 0)})


type ParserBuilder() =
    member this.Zero() = zero
    member this.Bind(parser: Parser<'A>, func: 'A -> Parser<'B>) : Parser<'B> = bind func parser
    member this.Return(item: 'A) : Parser<'A> = succeed item

let parser = ParserBuilder()


let attempt (parser: Parser<'A>) : Parser<'A> =
    fun input ->
        match parser input with
        | Error err -> Error { err with IsCommitted = false }
        | success -> success


let orElse (second: Parser<'A>) (first: Parser<'A>) : Parser<'A> =
    fun input ->
        match first input with
        | Error { Stack = _; IsCommitted = false } -> second input
        | it -> it


let skip (skipParse: Parser<'B>) (parse: Parser<'A>) : Parser<'A> =
    parse 
    |> map (fun it _skipedItem -> it) 
    |> apply skipParse


let skipRight (parser: Parser<'A>) (skipParser: Parser<'B>) : Parser<'A> =
    skipParser
    |> map (fun _skippedItem it -> it)
    |> apply parser


let inline ( *> ) skipParser parser = skipRight parser skipParser
let inline ( <* ) parser skipParser = skip skipParser parser

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


let letter: Parser<char> = satisfy Char.IsLetter 


let alphaNumeric: Parser<char> = satisfy Char.IsLetterOrDigit 


let whitespace: Parser<char> = satisfy Char.IsWhiteSpace


let rec many (parse: Parser<'A>) : Parser<list<'A>> =
    attempt(parser {
        let! x = parse
        let! xs = many parse

        return x :: xs
    }) |> orElse (succeed [])


let rec atLeast1 (parse: Parser<'A>) : Parser<list<'A>> =
    parser {
        let! x = parse
        let! xs = many parse

        return x :: xs
    }


let rec atLeast1SeparatedBy (separator: Parser<'B>) (parse: Parser<'A>) : Parser<list<'A>> =
    parser {
        let! x = parse
        let! xs = many (skipRight parse separator)

        return x :: xs
    }


let rec separatedBy (separator: Parser<'B>) (parse: Parser<'A>) : Parser<list<'A>> =
    attempt (atLeast1SeparatedBy separator parse) |> orElse (succeed [])


let word: Parser<list<char>> = many letter


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


let natural: Parser<int> = atLeast1 digit |> map (List.fold scale 0)


let between (brace: Parser<'B>) (parse: Parser<'A>) : Parser<'A> =
    parser {
        let! _ = brace
        let! item = parse
        let! _ = brace

        return item
    }
