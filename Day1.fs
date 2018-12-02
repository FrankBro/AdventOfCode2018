// Learn more about F# at http://fsharp.org

open System
open System.IO

let readLines = File.ReadLines

type Change =
    | Plus of int
    | Minus of int
with
    static member FromInt i =
        if i > 0 
        then Plus i
        else Minus -i

module Parser =
    open FParsec

    type Parser<'t> = Parser<'t, unit>

    let parseChange = pint32 |>> Change.FromInt

    let readOrThrow (parser: Parser<'t>) input : 't =
        match runParserOnString parser () "" input with
        | ParserResult.Success (result, state, pos) -> result
        | ParserResult.Failure (se, e, state) -> failwith "Parser error"

    let read input = readOrThrow (parseChange .>> eof) input

type State = {
    Frequency: int
    SeenFrequencies: Set<int>
    FirstSeenDouble: int option
    FirstIterationFrequency: int option
    Iteration: int
}
with
    static member New = {
        Frequency = 0
        SeenFrequencies = Set.singleton 0
        FirstSeenDouble = None
        FirstIterationFrequency = None
        Iteration = 0
    }

[<EntryPoint>]
let main argv =
    let lines = readLines "Day1.input"
    let changes = Seq.map Parser.read lines |> List.ofSeq
    let rec loop state xs =
        match xs, state.FirstSeenDouble with
        | _, Some _ when state.Iteration <> 0 -> state
        | [], Some _ -> state
        | [], None -> 
            let state =
                { state with
                    Iteration = state.Iteration + 1
                    FirstIterationFrequency = 
                        if state.Iteration = 0 
                        then Some state.Frequency 
                        else state.FirstIterationFrequency
                }
            loop state changes
        | change :: changes, _ ->
            let frequency =
                match change with
                | Plus i -> state.Frequency + i
                | Minus i -> state.Frequency - i
            let state =
                { state with
                    Frequency = frequency
                    SeenFrequencies = Set.add frequency state.SeenFrequencies
                    FirstSeenDouble =
                        match state.FirstSeenDouble with
                        | Some _ -> state.FirstSeenDouble
                        | None -> 
                            if Set.contains frequency state.SeenFrequencies
                            then Some frequency
                            else None
                }
            loop state changes
    let result = loop State.New changes
    printfn "%O" result
    0 // return an integer exit code
