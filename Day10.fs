module Day10

open System

open Util

type Point = {
    Id: int // To make sure they never merge in the map set
    X: int
    Y: int
    DX: int
    DY: int
}
with
    member x.Pos = x.X, x.Y
    override x.ToString () = sprintf "{ X = %d; Y = %d; DX = %d; DY = %d }" x.X x.Y x.DX x.DY

module Parser =
    open FParsec
    open FParsec.CharParsers
    open Util.Parser

    let parsePosition : Parser<int * int> =
        let a = str "position=<" >>. ws >>. pint32
        let b = str "," >>. ws >>. pint32 .>> str ">"
        a .>>. b

    let parseVelocity : Parser<int * int> =
        let a = str "velocity=<" >>. ws >>. pint32
        let b = str "," >>. ws >>. pint32 .>> str ">"
        a .>>. b

    let parsePoint : Parser<int -> Point> =
        pipe3 parsePosition (str " ") parseVelocity (fun (x, y) _ (dx, dy) ->
            fun id -> { Id = id; X = x; Y = y; DX = dx; DY = dy }
        )

    let read input = Parser.readOrThrow (parsePoint .>> eof) input

type Action =
    | MoveX of int
    | MoveY of int
    | Step of int
    | Draw
    | Locate
    // | Closer

module Console =
    open FParsec
    open Util.Parser

    let parseMoveX = str "x " >>. pint32 |>> MoveX
    let parseMoveY = str "y " >>. pint32 |>> MoveY
    let parseStep = str "s " >>. pint32 |>> Step
    let parseDraw = str "d" >>. preturn Draw
    let parseLocate = str "l" >>. preturn Locate
    // let parseCloser = str "c" >>. preturn Closer

    let parseCommand = attempt parseMoveX <|> attempt parseMoveY <|> attempt parseStep <|> attempt parseDraw <|> attempt parseLocate //<|> attempt parseCloser

    let read input = Parser.readOrThrow (parseCommand .>> eof) input

let all () =
    let lines = readLines "Day10.input"
    let points =
        lines
        |> Seq.mapi (fun i line ->
            let mkPoint = Parser.read line
            mkPoint i
        )
    let rec loop step (x, y) points =
        let action = 
            Console.ReadLine()
            |> Console.read
        match action with
        | Locate ->
            points
            |> Map.iter (fun _ points ->
                points
                |> Set.iter (printfn "%O")
            )
            points
            |> Seq.minBy (fun (KeyValue((x2, y2), point)) ->
                distance x y x2 y2
            )
            |> printfn "Step: %d, X: %d, Y: %d, Closest: %O" step x y
            loop step (x, y) points
        | MoveX dx -> loop step (x + dx, y) points
        | MoveY dy -> loop step (x, y + dy) points
        | Step count ->
            (Map.empty, points)
            ||> Map.fold (fun newPoints (x, y) points ->
                (newPoints, points)
                ||> Set.fold (fun newPoints point ->
                    let newPoint = 
                        { point with
                            X = point.X + (point.DX * count)
                            Y = point.Y + (point.DY * count)
                        }
                    newPoints
                    |> Map.updateWith newPoint.Pos (fun newPoints -> Set.add newPoint newPoints) (Set.singleton newPoint)
                )
            )
            |> loop (step + count) (x, y)
        | Draw ->
            for y in [x-50..x+50] do
                for x in [y-50..y+50] do
                    match points |> Map.tryFind (x, y) with
                    | None -> printf "."
                    | Some _ -> printf "#"
                printfn ""
            loop step (x, y) points
    printfn "Go"
    points
    |> Seq.map (fun point ->
        point.Pos, Set.singleton point
    )
    |> Map.ofSeq
    |> loop 0 (0, 0)
