module Day4

open System

open Util

type Entry =
    | WakesUp of DateTime
    | FallsAsleep of DateTime
    | BeginsShift of int * DateTime
with
    member x.DateTime =
        match x with
        | WakesUp t
        | FallsAsleep t
        | BeginsShift (_, t) -> t


module Parser =
    open FParsec

    open Util.Parser

    let parseTime : Parser<DateTime> = str "[" >>. many1Chars (noneOf "]") .>> str "]" |>> DateTime.Parse

    let parseWakesUp = str "wakes up" |>> fun _ -> WakesUp
    let parseFallsAsleep = str "falls asleep" |>> fun _ -> FallsAsleep
    let parseBeginsShift = str "Guard #" >>. pint32 .>> str " begins shift" |>> fun guard -> (fun t -> BeginsShift (guard, t))

    let parseEntry = choice [parseWakesUp; parseFallsAsleep; parseBeginsShift ]

    let parseTimedEntry = 
        parseTime .>> str " " .>>. parseEntry
        |>> (fun (t, e) -> e t)

    let read input = Parser.readOrThrow (parseTimedEntry .>> eof) input

type Shift = {
    Guard: int
    Asleep: Set<int>
}
with
    static member New guard = {
        Guard = guard
        Asleep = Set.empty
    }

let getShifts () =
    let lines = readLines "Day4.input"
    let entries = Seq.map Parser.read lines
    let sorted = 
        entries
        |> Seq.sortBy (fun entry -> entry.DateTime) 
    let rec loop oShift shifts xs =
        match oShift, xs with
        // Impossible
        | None, [] 
        | None, WakesUp _ :: _
        | None, FallsAsleep _ :: _ -> impossible ()
        // Done
        | Some shift, [] -> List.rev (shift :: shifts)
        // First shift
        | None, BeginsShift (guard, _) :: xs ->
            let newShift = Shift.New guard
            loop (Some newShift) shifts xs
        // New shift
        | Some shift, BeginsShift (guard, _) :: xs ->
            let newShift = Shift.New guard
            loop (Some newShift) (shift :: shifts) xs
        // Nap
        | Some shift, FallsAsleep t1 :: WakesUp t2 :: xs ->
            let asleep = set [t1.Minute..t2.Minute-1]
            let shift = { shift with Asleep = Set.union shift.Asleep asleep }
            loop (Some shift) shifts xs
        // Impossible
        | Some _, FallsAsleep _ :: _
        | Some _, WakesUp _ :: _ -> impossible ()
    let shifts =
        sorted
        |> List.ofSeq
        |> loop None []
    shifts 
    
let part1 () =
    let shifts = getShifts ()
    let guard =
        (Map.empty, shifts)
        ||> List.fold (fun state shift ->
            let currentSleep = Set.count shift.Asleep
            state
            |> Map.updateWith shift.Guard (fun previousSleep -> previousSleep + currentSleep) currentSleep
        )
        |> Seq.maxBy value
        |> key
    let minute =
        shifts
        |> List.filter (fun shift -> shift.Guard = guard)
        |> List.fold (fun state shift ->
            (state, shift.Asleep)
            ||> Seq.fold (fun state minute ->
                state
                |> Map.updateWith minute inc 1
            )
        ) Map.empty
        |> Seq.maxBy value
        |> key
    guard * minute

let part2 () =
    let shifts = getShifts ()
    let guard, minute =
        (Map.empty, shifts)
        ||> List.fold (fun state shift ->
            let guardState =
                state
                |> Map.tryFind shift.Guard
                |> Option.defaultValue Map.empty
            let guardState =
                (guardState, shift.Asleep)
                ||> Set.fold (fun state minute ->
                    state
                    |> Map.updateWith minute inc 1
                )
            state
            |> Map.add shift.Guard guardState
        )
        |> Seq.filter (fun (KeyValue(guard, distribution)) -> not <| Map.isEmpty distribution)
        |> Seq.map (fun (KeyValue(guard, distribution)) ->
            let (KeyValue(mostAsleepMinute, amount)) =
                distribution
                |> Seq.maxBy value
            (guard, mostAsleepMinute), amount
        )
        |> Seq.maxBy snd
        |> fst
    guard * minute
