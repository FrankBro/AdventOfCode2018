module Util

open System.IO

let readLines = File.ReadLines

let (|>!) x f = f x; x

let inc = (+) 1

let impossible () = failwith "impossible"

let key (KeyValue(k, _)) = k
let value (KeyValue(_,v)) = v

let distance x1 x2 y1 y2 = sqrt((float (x2 - x1)) ** 2. + (float (y2 - y1)) ** 2.)

module Map =
    let updateWith key fn init map =
        let value =
            map
            |> Map.tryFind key
            |> Option.map fn
            |> Option.defaultValue init
        Map.add key value map

    let update key fn map =
        let value =
            map
            |> Map.find key
            |> fn
        Map.add key value map
    
    let values m =
        m
        |> Map.toList
        |> List.map snd
    
module String =
    let filteri fn (s: string) =
        let chars =
            s.ToCharArray()
            |> Array.mapi (fun i c -> i, c)
            |> Array.filter (fun (i, c) -> fn i c)
            |> Array.map snd
        new string(chars)

    let toCharList (s: string) = s.ToCharArray() |> Array.toList

module List =
    let pop n xs =
        let rec loop n state xs =
            match n, xs with
            | 0, _ -> List.rev state, xs
            | n, x :: xs -> loop (n - 1) (x :: state) xs
            | n, [] -> impossible ()
        loop n [] xs

module Parser =
    open FParsec
    open FParsec.CharParsers

    type Parser<'t> = Parser<'t, unit>

    let str s = pstring s
    let ws = spaces

    let readOrThrow (parser: Parser<'t>) input : 't =
        match runParserOnString parser () "" input with
        | ParserResult.Success (result, state, pos) -> result
        | ParserResult.Failure (se, e, state) -> failwith "Parser error"
