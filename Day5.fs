module Day5

open System

open Util

let reduce (chars: char list) =
    let rec loop state (chars: char list) =
        match chars with
        | [] -> List.rev state
        | a :: b :: rest ->
            if (int a) = (int b - 32)
            then loop state rest
            elif (int a - 32) = (int b)
            then loop state rest
            else loop (a :: state) (b :: rest)
        | x :: xs -> 
            loop (x :: state) xs
    loop [] chars

let getPolymer () =
    let lines = readLines "Day5.input"
    let line = Seq.head lines
    let chars = 
        line.ToCharArray()
        |> List.ofArray
    chars

let reducePolymer polymer =
    let rec loop (chars: char list) =
        let reduced = reduce chars
        if reduced.Length <> chars.Length then
            loop reduced
        else
            reduced
    let remain = loop polymer
    remain.Length

let part1 () =
    let polymer = getPolymer ()
    reduce polymer

let part2 () =
    let polymer = getPolymer ()
    let without =
        ['a'..'z']
        |> List.map (fun c ->
            polymer
            |> List.filter (fun p -> Char.ToLower(p) <> c)
        )
    let min =
        without 
        |> List.map reducePolymer
        |> List.min
    min
