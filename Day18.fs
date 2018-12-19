module Day18

open Util
open System.Collections.Generic

type Tile =
    | Open
    | Tree
    | Lumberyard
with
    override x.ToString () =
        match x with
        | Open -> "."
        | Tree -> "|"
        | Lumberyard -> "#"

module Parser =
    open FParsec
    open Util.Parser

    let parseOpen = str "." >>. preturn Open
    let parseTree = str "|" >>. preturn Tree
    let parseLumberyard = str "#" >>. preturn Lumberyard
    let parseTile = attempt parseOpen <|> attempt parseTree <|> attempt parseLumberyard

    let parseTiles = many1 parseTile

    let readTiles input = Parser.readOrThrow (parseTiles .>> eof) input

let adjacent (x0, y0) =
    [y0-1..y0+1]
    |> List.collect (fun y ->
        [x0-1..x0+1]
        |> List.map (fun x -> x, y)
    )
    |> List.filter (fun (x, y) -> 
        (x = x0 && y = y0)
        |> not
    )

let countAdjacent pos tile map =
    adjacent pos
    |> List.filter (fun pos ->
        map
        |> Map.tryFind pos
        |> Option.exists (fun otherTile -> tile = otherTile)
    )
    |> List.length

let next pos tile map =
    match tile with
    | Open -> if countAdjacent pos Tree map > 2 then Tree else Open
    | Tree -> if countAdjacent pos Lumberyard map > 2 then Lumberyard else Tree
    | Lumberyard ->
        let lumberyardCount = countAdjacent pos Lumberyard map
        let treeCount = countAdjacent pos Tree map
        if lumberyardCount > 0 && treeCount > 0 then Lumberyard else Open

let printMap x y map =
    for y in [0..y] do
        for x in [0..x] do
            map
            |> Map.find (x, y)
            |> printf "%O"
        printfn ""

let part1 () =
    let lines = readLines "Day18.input"
    let map =
        lines
        |> Seq.mapi (fun y line ->
            let tiles = Parser.readTiles line
            tiles
            |> List.mapi (fun x tile ->
                (x, y), tile
            )
        )
        |> Seq.collect id
        |> Map.ofSeq
    let rec loop map n =
        match n with
        | 0 -> map
        | n ->
            let next =
                (Map.empty, map)
                ||> Map.fold (fun nextMap pos tile ->
                    let next = next pos tile map
                    nextMap
                    |> Map.add pos next
                )
            loop next (n - 1)
    let final = loop map 10
    let treeCount = final |> Map.filter (fun pos tile -> tile = Tree) |> Map.count
    let lumberyardCount = final |> Map.filter (fun pos tile -> tile = Lumberyard) |> Map.count
    treeCount * lumberyardCount
