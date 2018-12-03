module Day3

open Util

type Claim = {
    Id: int
    Position: int * int
    Dimension: int * int
}
with
    override x.ToString () = sprintf "#%d @ %d,%d: %dx%d" x.Id x.X x.Y x.W x.H
    member x.X = fst x.Position
    member x.Y = snd x.Position
    member x.W = fst x.Dimension
    member x.H = snd x.Dimension

let covers (claim: Claim) =
    let xs = [claim.X..claim.X+claim.W-1]
    let ys = [claim.Y..claim.Y+claim.H-1]
    xs
    |> List.collect (fun x ->
        ys
        |> List.map (fun y -> x, y)
    ) 
    |> set

module Parser =
    open FParsec

    open Util.Parser

    let parseClaim =
        let id = str "#" >>. pint32
        let position = str " @ " >>. pint32 .>>. (str ","  >>. pint32)
        let dimension = str ": " >>. pint32 .>>. (str "x" >>. pint32)
        pipe3 id position dimension (fun id position dimension ->
            {
                Id = id
                Position = position
                Dimension = dimension
            }
        )

    let read input = Parser.readOrThrow (parseClaim .>> eof) input

let part1 () =
    let lines = readLines "Day3.input"
    let claims = Seq.map Parser.read lines
    let rec loop covered crossed claims =
        match claims with
        | [] -> Set.count crossed
        | claim :: claims ->
            let claimCovered = covers claim
            let claimCrossed = Set.intersect claimCovered covered 
            let covered = Set.union covered claimCovered
            let crossed = Set.union crossed claimCrossed
            loop covered crossed claims
    claims
    |> List.ofSeq
    |> loop Set.empty Set.empty

let part2 () =
    let lines = readLines "Day3.input"
    let claims = 
        Seq.map Parser.read lines
        |> List.ofSeq
    let rec loop covered crossed claims =
        match claims with
        | [] -> crossed
        | claim :: claims ->
            let claimCovered = covers claim
            let claimCrossed = Set.intersect claimCovered covered 
            let covered = Set.union covered claimCovered
            let crossed = Set.union crossed claimCrossed
            loop covered crossed claims
    let crossed =
        claims
        |> loop Set.empty Set.empty
    claims
    |> List.pick (fun claim ->
        let claimCovered = covers claim
        let claimCrossed = Set.intersect claimCovered crossed
        if Set.isEmpty claimCrossed then
            Some claim.Id
        else
            None
    )
