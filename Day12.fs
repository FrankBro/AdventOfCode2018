module Day12

open System

open Util

type Input =
    | InitialState of string
    | Change of string * string

module Parser =
    open FParsec
    open Util.Parser

    let parseInitialState =
        str "initial state: " >>. many1 anyChar |>> (Array.ofList >> String >> InitialState)

    let parseChange =
        many1 (pchar ".")
