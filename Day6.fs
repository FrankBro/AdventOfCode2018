module Day6

open Util
open FParsec

type [<Measure>] id

type Position = {
    X: int
    Y: int
}

type Point = {
    Id: int<id>
    Position: Position
}
with
    member x.X = x.Position.X
    member x.Y = x.Position.Y
    static member SX p = p.Position.X
    static member SY p = p.Position.Y

module Parser =
    open FParsec

    open Util.Parser

    let parsePosition = 
        pint32 .>> str ", " .>>. pint32
        |>> fun (x, y) -> { X = x; Y = y }

    let read input = Parser.readOrThrow (parsePosition .>> eof) input

let part1 () =
    let lines : seq<string> = readLines "Day6.input"
    let points =
        lines
        |> Seq.mapi (fun i line -> { Id = i * 1<id>; Position = Parser.read line})
    let minX = points |> Seq.minBy (fun point -> point.Position.X) |> Point.SX
    let minY = points |> Seq.minBy (fun point -> point.Position.Y) |> Point.SY
    let maxX = points |> Seq.maxBy (fun point -> point.Position.X) |> Point.SX
    let maxY = points |> Seq.maxBy (fun point -> point.Position.Y) |> Point.SY
    let xs = [minX-1..maxX+1]
    let ys = [minY-1..maxY+1]
    let map =
        seq {
            for x in xs do
                for y in ys do
                    yield (x, y)
        }
    let final =
        map
        |> Seq.map (fun (x, y) ->
            let x = float x
            let y = float y
            let distances =
                points
                |> Seq.map (fun point ->
                    let dx = abs(float point.X - x)
                    let dy = abs(float point.Y - y)
                    let x2 = dx * dx
                    let y2 = dy * dy
                    point.Id, sqrt(x2 + y2)
                )
            let minId, minDst =
                distances
                |> Seq.minBy snd
            let count =
                distances
                |> Seq.sumBy (fun (_, d) -> if d = minDst then 1 else 0)
            if count = 1 then
                (x, y), Some minId
            else
                (x, y), None
        )
        |> Map.ofSeq
    let filtered =
        final
        |> Seq.choose (fun (KeyValue((x, y), oId)) ->
            let x = int x
            let y = int y
            match oId with
            | Some id when (x <= minX || x >= maxX || y <= minY || y >= maxY) -> Some id
            | _ -> None
        )
        |> set
    let points =
        points
        |> Seq.choose (fun point ->
            if Set.contains point.Id filtered then
                None
            else
                let x = point.X
                let y = point.Y
                let sum : int =
                    final
                    |> Seq.sumBy (fun (KeyValue(_, v)) -> if Some point.Id = v && (x >= minX && x <= maxX) && (y >= minY && y <= maxY) then 1 else 0)
                Some sum
        )
    points
    |> Seq.max
