module Day12

open System

open Util
open System.Threading

type Tile =
    | Empty
    | Pot
with
    override x.ToString () =
        match x with
        | Empty -> "."
        | Pot -> "#"

module Parser =
    open FParsec
    open FParsec.CharParsers
    open Util.Parser

    let parseEmpty = pchar '.' >>. preturn Empty
    let parsePot = pchar '#' >>. preturn Pot

    let parseTile = parseEmpty <|> parsePot

    let parseInitialState =
        str "initial state: " >>. many1 parseTile

    let parseChange =
        many1 parseTile .>> str " => " .>>. parseTile

    let readInitialState input = Parser.readOrThrow (parseInitialState .>> eof) input
    let readChange input = Parser.readOrThrow (parseChange .>> eof) input

let tilesToString (tiles: Tile list) =
    tiles
    |> List.map string
    |> String.concat ""

let printState n (map: Map<int, Tile>) =
    printf "%02d: " n
    for i in [-50..50] do
        map
        |> Map.tryFind i
        |> Option.defaultValue Empty
        |> printf "%O" 
    printfn ""

let matches pattern i state =
    let indexedPattern =
        pattern
        |> List.mapi (fun i tile -> i - 2, tile)
    indexedPattern
    |> List.forall (fun (j, tile) ->
        let stateTile =
            state
            |> Map.tryFind (i + j)
            |> Option.defaultValue Empty
        tile = stateTile
    )
    // |>! fun b ->
    //     printfn "i = %d, pattern = %s, state = %O" i (tilesToString pattern) (state |> Map.values |> tilesToString)

let minMax (map: Map<int, Tile>) =
    let pots =
        map
        |> Map.filter (fun _ tile -> tile = Pot)
        |> Map.keys
    let min =
        pots
        |> List.min
    let max =
        pots
        |> List.max
    min, max

let getData () =
    let lines = 
        readLines "Day12.input"
        |> List.ofSeq
    let initialState, changes =
        let rec loop oInitialState changes lines = 
            match oInitialState, lines with
            | Some initialState, [] -> 
                let map =
                    initialState
                    |> List.mapi (fun i tile -> i, tile)
                    |> Map.ofList
                map, List.rev changes
            | None, line :: lines ->
                let initialState = Parser.readInitialState line
                loop (Some initialState) changes lines
            | _, "" :: lines ->
                loop oInitialState changes lines
            | _, line :: lines ->
                let change = Parser.readChange line
                loop oInitialState (change :: changes) lines
            | _ -> impossible ()
        loop None [] lines
    initialState, changes

let getGeneration state changes n =
    let rec loop state = function
        | 0 -> 
            // printState 0 state
            state
        | n -> 
            // printState n state
            let min, max = minMax state
            let next =
                (Map.empty, [min-2..max+2])
                ||> List.fold (fun next i ->
                    let rec loop next = function
                        | [] -> next |> Map.add i Empty
                        | (pattern, result) :: changes ->
                            if matches pattern i state then
                                // printfn "pattern %s matched for i = %d" (tilesToString pattern) i
                                next |> Map.add i result
                            else
                                loop next changes
                    loop next changes
                )
            loop next (n - 1)
    let final = loop state n
    final

let sum map =
    map
    |> Seq.sumBy (fun (KeyValue(i, tile)) ->
        match tile with
        | Empty -> 0
        | Pot -> i
    )

let part1 () = 
    let initialState, changes = getData ()
    getGeneration initialState changes 20
    |> sum
            
let part2 () = 
    let initialState, changes = getData ()
    (initialState, [1..100])
    ||> List.fold (fun state i ->
        printfn "%d" i
        getGeneration state changes 500000000
    )
