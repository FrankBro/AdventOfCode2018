module Day4

open System

open Util

type Entry =
    | WakesUp
    | FallsAsleep
    | BeginsShift of int

module Parser =
    open FParsec

    open Util.Parser

    let parseTime : Parser<DateTime> = str "[" >>. many1Chars (noneOf "]") .>> str "]" |>> DateTime.Parse

    let parseWakesUp = str "wakes up" |>> fun _ -> WakesUp
    let parseFallsAsleep = str "falls asleep" |>> fun _ -> FallsAsleep
    let parseBeginsShift = str "Guard #" >>. pint32 .>> str " begins shift" |>> BeginsShift

    let parseEntry = choice [parseWakesUp; parseFallsAsleep; parseBeginsShift ]

    let parseTimedEntry = parseTime .>> str " " .>>. parseEntry

    let read input = Parser.readOrThrow (parseTimedEntry .>> eof) input

let part1 () =
    let lines = readLines "Day4.input"
    let timedEntries = Seq.map Parser.read lines
    let sorted = Seq.sortBy fst timedEntries