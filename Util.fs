module Util

open System.IO

let readLines = File.ReadLines

module Map =
    let updateWith key fn init map =
        let value =
            map
            |> Map.tryFind key
            |> Option.map fn
            |> Option.defaultValue init
        Map.add key value map
    
module String =
    let filteri fn (s: string) =
        let chars =
            s.ToCharArray()
            |> Array.mapi (fun i c -> i, c)
            |> Array.filter (fun (i, c) -> fn i c)
            |> Array.map snd
        new string(chars)

    let toCharList (s: string) = s.ToCharArray() |> Array.toList
