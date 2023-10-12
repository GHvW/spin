namespace Spin

open System

type Location = { Input: ReadOnlyMemory<char>; Offset: int }

module Location =
    
    let init (input: string) : Location =
        { Input = input.AsMemory(); Offset = 0 }


    let inline advanceBy n location =
        { location with Offset = location.Offset + n }
