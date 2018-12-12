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
    // let emptyGrid = makeEmptyGrid 1 1 300
    let emptyGrid = makeEmptyGrid min min (max - n + 1)
    emptyGrid
    |> Array.Parallel.map (fun (x, y) ->
        let square = makeEmptyGrid x y n
        // let anyOutside =
        //     square
        //     |> Array.exists (fun (x, y) -> x < min || x > max || y < min || y > max)
        // if anyOutside then (x, y), 0 else
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
    let (x, y), sum =  find 3 grid
    (x, y)
    
let part2 () =
    let grid = getGrid ()
    let x, y, n =
        [|1..300|]
        |> Array.Parallel.map (fun n ->
            let (x, y), sum = find n grid
            printfn "%d" n
            (x, y, n), sum
        )
        |> Array.maxBy snd
        |> fst
    (x, y, n)
