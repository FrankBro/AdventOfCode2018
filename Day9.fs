module Day9

open Util

type Data = {
    Players: int
    Points: int
}

module Parser =
    open FParsec
    open Util.Parser

    let parseInput = 
        let pa = pint32 .>> str "players; last marbe is worth "
        let pb = pint32 .>> str "points"
        pipe2 pa pb (fun players points -> { Players = players; Points = points })

    let read input = Parser.readOrThrow (parseInput .>> eof) input

let part1 () =
    ()