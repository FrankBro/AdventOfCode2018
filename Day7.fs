module Day7

open Util

type Step =
    | Step of string * string

module Parser =
    open FParsec

    open Util.Parser

    let supper = upper |>> string

    let parseStep : Parser<Step> =
        str "Step " >>. supper .>> str " must be finished before step " .>>. supper .>> str " can begin."
        |>> Step

    let read input = Parser.readOrThrow (parseStep .>> eof) input

let data () =
    let lines = readLines "Day7.input"
    let steps = Seq.map Parser.read lines
    let allLetters =
        (Set.empty, steps)
        ||> Seq.fold (fun letters (Step (a, b)) ->
            letters
            |> Set.add a
            |> Set.add b
        )
    let dependencies =
        allLetters
        |> Seq.map (fun letter -> 
            let dependencies =
                steps
                |> Seq.choose (fun (Step (a, b)) ->
                    if b = letter then
                        Some a
                    else
                        None
                )
                |> set
            letter, dependencies
        )
        |> Map.ofSeq
    let initial =
        dependencies
        |> Seq.minBy (fun (KeyValue (_, dependencies)) ->
            Set.count dependencies
        )
        |> key
    initial, dependencies
        
let part1 () =
    let initial, dependencies = data ()
    let rec loop (path: List<string>) =
        let pathSet = set path
        let unlocked =
            dependencies
            |> Seq.choose (fun (KeyValue(unlocked, dependencies)) ->
                if Set.isSubset dependencies pathSet then
                    Some unlocked
                else
                    None
            )
            |> Seq.filter (fun unlocked ->
                path
                |> List.exists ((=) unlocked)
                |> not
            )
            |> List.ofSeq
        printfn "path: %O, unlocked: %O" path unlocked
        match unlocked with
        | [] -> path
        | [x] -> loop (x :: path)
        | xs ->
            let x =
                xs 
                |> List.minBy id
            loop (x :: path)
    loop [initial]
    |> List.rev
    |> String.concat ""

let part2 () =
    let initial, dependencies = data ()
    let workers = 
        [1..5]
        |> List.map (fun id -> id, (0, None))
        |> Map.ofSeq
    let rec loop (tick: int) (workers: Map<int, int * string option>) (path: Set<string>) =
        let (path, workers) =
            ((path, workers), workers)
            ||> Map.fold (fun (path, workers) id (t, oNext) ->
                match oNext with
                | Some next when t = tick -> 
                    Set.add next path, Map.add id (t, None) workers
                | _ -> 
                    path, Map.add id (t, oNext) workers
            )
        let oWorker =
            workers
            |> Map.tryPick (fun id (t, oNext) -> if Option.isNone oNext then Some id else None)
        match oWorker with
        | None -> 
            printfn "step time"
            loop (tick + 1) workers path
        | Some worker ->
            let pathSet = set path
            let unlocked =
                dependencies
                |> Seq.choose (fun (KeyValue(unlocked, dependencies)) ->
                    if Set.isSubset dependencies pathSet then
                        Some unlocked
                    else
                        None
                )
                |> Seq.filter (fun unlocked ->
                    path
                    |> Set.contains unlocked
                    |> not
                )
                |> Seq.filter (fun unlocked ->
                    workers
                    |> Map.exists (fun _ (_, oNext) -> Some unlocked = oNext)
                    |> not
                )
                |> List.ofSeq
            let sendWorker (letter: string) =
                printfn "send %d to %s" worker letter
                workers
                |> Map.update worker (fun (tick, _) -> tick + 60 + (int letter.[0] - 64), Some letter)
            match unlocked with
            | [] -> 
                
                workers
            | [x] -> loop tick (sendWorker x) path
            | xs ->
                let x =
                    xs 
                    |> List.minBy id
                loop tick (sendWorker x) path
    loop 0 workers (Set.singleton initial)
    |> Seq.maxBy (fun (KeyValue(_, t)) -> t)
