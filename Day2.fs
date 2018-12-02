module Day2

open Util
open System

type Checksum = {
    HasDouble: bool
    HasTriple: bool
}

let extractChecksum (boxId: string) =
    let map =
        (Map.empty, boxId.ToCharArray())
        ||> Array.fold (fun map char ->
            map
            |> Map.updateWith char (fun i -> i + 1) 1
        )
    { 
        HasDouble = Map.exists (fun _ count -> count = 2) map
        HasTriple = Map.exists (fun _ count -> count = 3) map
    }

type Output = {
    Checksum: int
    Fabric: string
}

let day2 =
    let lines = readLines "Day2.input"
    // Part 1
    let checksums = Seq.map extractChecksum lines
    let doubles = Seq.sumBy (fun checksum -> if checksum.HasDouble then 1 else 0) checksums
    let triples = Seq.sumBy (fun checksum -> if checksum.HasTriple then 1 else 0) checksums
    let checksum = doubles * triples
    // Part 2
    let fabric1, fabric2 =
        lines
        |> Seq.pick (fun line1 ->
            let chars1 = String.toCharList line1
            lines
            |> Seq.tryPick (fun line2 ->
                let chars2 = String.toCharList line2
                let rec loop diff chars1 chars2 =
                    match chars1, chars2 with
                    | [], [] when diff = 1 -> Some (line1, line2)
                    | [], [] -> None
                    | char1 :: chars1, char2 :: chars2 ->
                        let diff =
                            if char1 = char2
                            then diff
                            else diff + 1
                        loop diff chars1 chars2
                    | _, _ -> failwith "Should never happen"
                loop 0 chars1 chars2
            )
        )
    let fabric =
        fabric1
        |> String.filteri (fun i c1 ->
            c1 = (fabric2.Chars i)
        )
    //
    { Checksum = checksum; Fabric = fabric }
