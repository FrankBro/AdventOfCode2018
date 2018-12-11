module Day8

open Util

type Node = 
    | Node of children: Node list * data: int list
with
    override x.ToString () =
        let (Node (children, data)) = x
        let children = children |> List.map string |> String.concat ", "
        let data = data |> List.map string |> String.concat ", "
        sprintf "Node ([%s], [%s])" children data

module Parser =
    open FParsec
    open Util.Parser

    let parseNode : Parser<int list> = sepBy1 pint32 (str " ")

    let read input = Parser.readOrThrow (parseNode .>> eof) input

let rec parseNode raw : Node * int list =
    match raw with
    | nchild :: ndata :: raw ->
        let rec loop n nodes raw =
            match n with
            | 0 -> nodes, raw
            | n ->
                let node, raw = parseNode raw
                loop (n - 1) (node :: nodes) raw
        let children, raw = loop nchild [] raw
        let children = List.rev children
        let data, raw = List.pop ndata raw
        Node (children, data), raw
    | _ -> impossible ()

let rec part1Sum (Node (children, data)) : int =
    let sumChildren =
        children
        |> List.sumBy part1Sum
    let sumData =
        data
        |> List.sum
    sumChildren + sumData

let getNodes () =
    let lines = readLines "Day8.input"
    let raw = 
        Seq.map Parser.read lines
        |> List.ofSeq
    let parsed = 
        raw
        |> List.map (parseNode >> fst)
    parsed

let part1 () =
    let nodes = getNodes ()
    let sum =  
        nodes
        |> List.sumBy part1Sum
    sum

let rec part2Sum ((Node (children, data)) as node) : int =
    match children with
    | [] -> List.sum data
    | children ->
        data
        |> List.sumBy (fun n ->
            match n with
            | 0 -> 0
            | _ -> 
                children
                |> List.tryItem (n - 1)
                |> Option.map part2Sum
                |> Option.defaultValue 0
        )

let part2 () =
    let nodes = getNodes ()
    let sum =
        nodes
        |> List.sumBy part2Sum
    sum
