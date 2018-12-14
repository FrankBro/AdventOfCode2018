module Day11

open Util
open System.Threading.Tasks
open System

let input = 6878

let min = 1
let max = 300

let makeEmptyGrid x y n =
    [|x..x+n-1|]
    |> Array.Parallel.collect (fun x ->
        [|y..y+n-1|]
        |> Array.Parallel.map (fun y -> x, y)
    )

let getGrid () =
    let emptyGrid = makeEmptyGrid 1 1 300
    let grid =
        emptyGrid
        |> Array.Parallel.map (fun (x, y) ->
            let rackId = x + 10
            let prePowerLevel = (((rackId * y) + input) * rackId)
            let powerLevel = ((prePowerLevel / 100) % 10) - 5
            (x, y), powerLevel
        )
        |> Map.ofArray
    grid

let find n (grid: Map<int * int, int>) =
    let emptyGrid = makeEmptyGrid min min (max - n + 1)
    emptyGrid
    |> Array.Parallel.map (fun (x, y) ->
        let square = makeEmptyGrid x y n
        let powers =
            square
            |> Array.Parallel.map (fun (x, y) ->
                grid
                |> Map.find (x, y)
            )
        let sum = Array.sum powers
        Array.head square, sum
    )
    |> Array.maxBy snd

let part1 () =
    let grid = getGrid ()
    let (x, y), sum = find 3 grid
    (x, y)
    
let part2 () =
    let grid = getGrid ()
    let summedArea =
        (Map.empty, grid)
        ||> Map.fold (fun summedArea (x, y) power ->
            let get dx dy = summedArea |> Map.tryFind (x + dx, y + dy) |> Option.defaultValue 0
            let summed = power + get 0 -1 + get -1 0 - get -1 -1
            Map.add (x, y) summed summedArea
        )
    let x, y, n =
        [|1..300|]
        |> Array.map (fun n ->
            let emptyGrid = makeEmptyGrid min min (max - n + 1)
            emptyGrid 
            |> Array.Parallel.map (fun (x, y) ->
                let get dx dy = summedArea |> Map.tryFind (x + dx, y + dy) |> Option.defaultValue 0
                let total = get 0 0 - get 0 -n - get -n 0 + get -n -n
                (x, y, n), total
            )
            |> Array.maxBy snd
        )
        |> Array.maxBy snd
        |> fst
    (x + 1 - n, y + 1 - n, n)
