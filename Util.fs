module Util

open System.IO

let readLines = File.ReadLines

let (|>!) x f = f x; x

let inc = (+) 1

let impossible () = failwith "impossible"

let key (KeyValue(k, _)) = k
let value (KeyValue(_,v)) = v

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

module Parser =
    open FParsec

    type Parser<'t> = Parser<'t, unit>

    let str s = pstring s

    let readOrThrow (parser: Parser<'t>) input : 't =
        match runParserOnString parser () "" input with
        | ParserResult.Success (result, state, pos) -> result
        | ParserResult.Failure (se, e, state) -> failwith "Parser error"
