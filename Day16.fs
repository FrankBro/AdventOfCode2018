module Day16

open Util

type [<Measure>] Reg
type [<Measure>] Val
type [<Measure>] Ign

type Registers = {
    R0: int<Val>
    R1: int<Val>
    R2: int<Val>
    R3: int<Val>
}
with
    static member New = { R0 = 0<Val>; R1 = 0<Val>; R2 = 0<Val>; R3 = 0<Val> }
    member x.Set (i: int<Reg>) (value: int<Val>) = 
        match i with
        | 0<Reg> -> { x with R0 = value }
        | 1<Reg> -> { x with R1 = value }
        | 2<Reg> -> { x with R2 = value }
        | 3<Reg> -> { x with R3 = value }
        | _ -> impossible ()
    member x.Get (i: int<Reg>) = 
        match i with
        | 0<Reg> -> x.R0
        | 1<Reg> -> x.R1
        | 2<Reg> -> x.R2
        | 3<Reg> -> x.R3
        | _ -> impossible ()

type Op =
    // Addition
    | Addr of A: int<Reg> * B: int<Reg> * C: int<Reg>
    | Addi of A: int<Reg> * B: int<Val> * C: int<Reg>
    // Multiplication
    | Mulr of A: int<Reg> * B: int<Reg> * C: int<Reg>
    | Muli of A: int<Reg> * B: int<Val> * C: int<Reg>
    // Bitwise AND
    | Banr of A: int<Reg> * B: int<Reg> * C: int<Reg>
    | Bani of A: int<Reg> * B: int<Val> * C: int<Reg>
    // Bitwise OR
    | Borr of A: int<Reg> * B: int<Reg> * C: int<Reg>
    | Bori of A: int<Reg> * B: int<Val> * C: int<Reg>
    // Assignment
    | Setr of A: int<Reg> * B: int<Ign> * C: int<Reg>
    | Seti of A: int<Val> * B: int<Ign> * C: int<Reg>
    // Comparison
    | Gtir of A: int<Val> * B: int<Reg> * C: int<Reg>
    | Gtri of A: int<Reg> * B: int<Val> * C: int<Reg>
    | Gtrr of A: int<Reg> * B: int<Reg> * C: int<Reg>
    // Equality
    | Eqir of A: int<Val> * B: int<Reg> * C: int<Reg>
    | Eqri of A: int<Reg> * B: int<Val> * C: int<Reg>
    | Eqrr of A: int<Reg> * B: int<Reg> * C: int<Reg>
with
    static member SetRegisters a b c x = 
        match x with
        | Addr _ -> Addr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>)
        | Addi _ -> Addi (a * 1<Reg>, b * 1<Val>, c * 1<Reg>)
        | Mulr _ -> Mulr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>)
        | Muli _ -> Muli (a * 1<Reg>, b * 1<Val>, c * 1<Reg>)
        | Banr _ -> Banr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>)
        | Bani _ -> Bani (a * 1<Reg>, b * 1<Val>, c * 1<Reg>)
        | Borr _ -> Borr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>)
        | Bori _ -> Bori (a * 1<Reg>, b * 1<Val>, c * 1<Reg>)
        | Setr _ -> Setr (a * 1<Reg>, b * 1<Ign>, c * 1<Reg>)
        | Seti _ -> Seti (a * 1<Val>, b * 1<Ign>, c * 1<Reg>)
        | Gtir _ -> Gtir (a * 1<Val>, b * 1<Reg>, c * 1<Reg>)
        | Gtri _ -> Gtri (a * 1<Reg>, b * 1<Val>, c * 1<Reg>)
        | Gtrr _ -> Gtrr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>)
        | Eqir _ -> Eqir (a * 1<Val>, b * 1<Reg>, c * 1<Reg>)
        | Eqri _ -> Eqri (a * 1<Reg>, b * 1<Val>, c * 1<Reg>)
        | Eqrr _ -> Eqrr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>)
    member x.ToNumber =
        match x with
        | Addr _ -> 0
        | Addi _ -> 1
        | Mulr _ -> 2
        | Muli _ -> 3
        | Banr _ -> 4
        | Bani _ -> 5
        | Borr _ -> 6
        | Bori _ -> 7
        | Setr _ -> 8
        | Seti _ -> 9
        | Gtir _ -> 10
        | Gtri _ -> 11
        | Gtrr _ -> 12
        | Eqir _ -> 13
        | Eqri _ -> 14
        | Eqrr _ -> 15

type RawOp = {
    Op: int
    A: int
    B: int
    C: int
}

let produceAll { A = a; B = b; C = c } =
    [
        Addr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>)
        Addi (a * 1<Reg>, b * 1<Val>, c * 1<Reg>)
        Mulr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>)
        Muli (a * 1<Reg>, b * 1<Val>, c * 1<Reg>)
        Banr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>)
        Bani (a * 1<Reg>, b * 1<Val>, c * 1<Reg>)
        Borr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>)
        Bori (a * 1<Reg>, b * 1<Val>, c * 1<Reg>)
        Setr (a * 1<Reg>, b * 1<Ign>, c * 1<Reg>)
        Seti (a * 1<Val>, b * 1<Ign>, c * 1<Reg>)
        Gtir (a * 1<Val>, b * 1<Reg>, c * 1<Reg>)
        Gtri (a * 1<Reg>, b * 1<Val>, c * 1<Reg>)
        Gtrr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>)
        Eqir (a * 1<Val>, b * 1<Reg>, c * 1<Reg>)
        Eqri (a * 1<Reg>, b * 1<Val>, c * 1<Reg>)
        Eqrr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>)
    ]

let apply (regs: Registers) op =
    match op with
    | Addr (a, b, c) -> regs.Set c (regs.Get a + regs.Get b)
    | Addi (a, b, c) -> regs.Set c (regs.Get a + b)
    | Mulr (a, b, c) -> regs.Set c (regs.Get a * regs.Get b / 1<Val>)
    | Muli (a, b, c) -> regs.Set c (regs.Get a * b / 1<Val>)
    | Banr (a, b, c) -> regs.Set c ((regs.Get a / 1<Val> &&& regs.Get b /1<Val>) * 1<Val>)
    | Bani (a, b, c) -> regs.Set c ((regs.Get a / 1<Val> &&& b /1<Val>) * 1<Val>)
    | Borr (a, b, c) -> regs.Set c ((regs.Get a / 1<Val> ||| regs.Get b /1<Val>) * 1<Val>)
    | Bori (a, b, c) -> regs.Set c ((regs.Get a / 1<Val> ||| b /1<Val>) * 1<Val>)
    | Setr (a, b, c) -> regs.Set c (regs.Get a)
    | Seti (a, b, c) -> regs.Set c a
    | Gtir (a, b, c) -> regs.Set c (if a > regs.Get b then 1<Val> else 0<Val>)
    | Gtri (a, b, c) -> regs.Set c (if regs.Get a > b then 1<Val> else 0<Val>)
    | Gtrr (a, b, c) -> regs.Set c (if regs.Get a > regs.Get b then 1<Val> else 0<Val>)
    | Eqir (a, b, c) -> regs.Set c (if a = regs.Get b then 1<Val> else 0<Val>)
    | Eqri (a, b, c) -> regs.Set c (if regs.Get a = b then 1<Val> else 0<Val>)
    | Eqrr (a, b, c) -> regs.Set c (if regs.Get a = regs.Get b then 1<Val> else 0<Val>)

type Sample = {
    Before: Registers
    Op: RawOp
    After: Registers
}

module Parser =
    open FParsec
    open Util.Parser

    let parseRegisters =
        str "[" >>. sepBy1 pint32 (str ", ") .>> str "]"
        |>> fun xs ->
            match xs |> List.map ((*) 1<Val>) with
            | [r0; r1; r2; r3] -> { R0 = r0; R1 = r1; R2 = r2; R3 = r3 }
            | _ -> impossible ()

    let parseOp =
        sepBy1 pint32 (str " ")
        |>> fun xs ->
            match xs with
            | [op; a; b; c] -> { Op = op; A = a; B = b; C = c }
            | _ -> impossible ()

    let parseBefore = str "Before: " >>. parseRegisters
    let parseAfter = str "After:  " >>. parseRegisters

    let readBefore input = Parser.readOrThrow (parseBefore .>> eof) input
    let readAfter input = Parser.readOrThrow (parseAfter .>> eof) input
    let readOp input = Parser.readOrThrow (parseOp .>> eof) input

type Next =
    | BeforeNext of int // whitelines saw
    | OpNext of Registers
    | AfterNext of Registers * RawOp
    | ListNext

type ParseState = {
    Next: Next
    Samples: Sample list
    Ops: RawOp list
}
with
    static member New = {
        Next = BeforeNext 0
        Samples = []
        Ops = []
    }

let getInput () =
    let lines = readLines "Day16.input"
    let state = 
        (ParseState.New, lines)
        ||> Seq.fold (fun state line ->
            match state.Next with
            | BeforeNext 3 -> { state with Next = ListNext }
            | BeforeNext n ->
                if line = "" then 
                    { state with Next = BeforeNext (n + 1) }
                else
                    let before = Parser.readBefore line
                    { state with Next = OpNext before }
            | OpNext before ->
                let op = Parser.readOp line   
                { state with Next = AfterNext (before, op) }
            | AfterNext (before, op) ->
                let after = Parser.readAfter line
                let sample = {
                    Before = before
                    Op = op
                    After = after
                }
                { state with 
                    Samples = sample :: state.Samples
                    Next = BeforeNext 0
                }
            | ListNext ->
                if line = "" then
                    state
                else
                    let op = Parser.readOp line
                    { state with Ops = op :: state.Ops }
        )
    let samples = List.rev state.Samples
    let ops = List.rev state.Ops
    samples, ops

let part1 () =
    let samples, ops = getInput ()
    let count =
        samples
        |> List.filter (fun sample ->
            let all = produceAll sample.Op
            let matches =
                all
                |> List.filter (fun op ->
                    apply sample.Before op = sample.After
                )
                |> List.length
            matches > 2
        )
        |> List.length
    count

let part2 () =
    let originalSamples, ops = getInput ()
    let rec loop seen (state: Map<int, Op>) samples =
        match samples with
        | [] when seen = Map.count state -> state
        | [] -> loop (Map.count state) state originalSamples
        | sample :: samples ->
            let matches =
                produceAll sample.Op
                |> List.filter (fun op ->
                    apply sample.Before op = sample.After
                )
                |> List.filter (fun op ->
                    state
                    |> Map.exists (fun _ stateOp ->
                        op.ToNumber = stateOp.ToNumber
                    )
                    |> not
                )
            match matches with
            | [onlyPossibility] ->
                let state =
                    state
                    |> Map.add sample.Op.Op onlyPossibility
                loop seen state samples
            | _ ->
                loop seen state samples
    let opMap = loop 0 Map.empty originalSamples
    for (KeyValue(rawOp, op)) in opMap do
        printfn "%d -> %O" rawOp op
    let registers =
        (Registers.New, ops)
        ||> List.fold (fun registers op ->
            let op =
                opMap
                |> Map.find op.Op
                |> Op.SetRegisters op.A op.B op.C
            apply registers op
        )
    printfn "%O" registers
    registers.R0
