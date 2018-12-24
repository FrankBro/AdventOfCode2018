module Day15

open Util

type Tile =
    | Open
    | Wall
    | Goblin
    | Elf

module Parser =
    open FParsec
    open Util.Parser

    let parseOpen = str "." >>. preturn Open
    let parseWall = str "#" >>. preturn Wall
    let parseGoblin = str "G" >>. preturn Goblin
    let parseElf = str "E" >>. preturn Elf
    let parseTile = attempt parseOpen <|> attempt parseWall <|> attempt parseGoblin <|> attempt parseElf
    let parseTiles = many1 parseTile
    
    let readTiles input = Parser.readOrThrow (parseTiles .>> eof) input

let adjacent (x, y) = [(x, y - 1); (x - 1, y); (x + 1, y); (x, y + 1)]

let isTileAdjacent pos tile map =
    adjacent pos
    |> List.exists (fun pos ->
        map
        |> Map.tryFind pos
        |> Option.exists ((=) tile)
    )

let isEnemyInRange pos tile map =
    match tile with
    | Goblin -> isTileAdjacent pos Elf map
    | Elf -> isTileAdjacent pos Goblin map
    | _ -> false

let getEnemyTiles tile map =
    match tile with
    | Goblin ->
        map
        |> Map.filter (fun _ tile -> tile = Elf)
    | Elf ->
        map
        |> Map.filter (fun _ tile -> tile = Elf)
    | _ -> Map.empty

let bestMove pos tile map =
    let tiles = getEnemyTiles tile map
    let shortestDistance =
        tiles
        |> Seq.minBy (fun (KeyValue(otherPos, otherTile)) ->
            failwith "todo"
        )
    failwith "todo"

let getMap () =
    let lines = readLines "Day15.input"
    let map =
        lines
        |> Seq.mapi (fun y line ->
            Parser.readTiles line
            |> List.mapi (fun x tile -> (x, y), tile)
        )
        |> Seq.collect id
        |> Map.ofSeq
    map

let testMove () =
    let lines = [
        "#######"
        "#E..G.#"
        "#...#.#"
        "#.G.#G#"
        "#######"
    ]
    let map =
        lines
        |> Seq.mapi (fun y line ->
            Parser.readTiles line
            |> List.mapi (fun x tile -> (x, y), tile)
        )
        |> Seq.collect id
        |> Map.ofSeq
    bestMove (1, 1) Elf map

let part1 () =
    let map = getMap ()
    ()
