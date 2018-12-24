module Day19

open Util
open Day16
open Day16

type [<Measure>] Reg
type [<Measure>] Val
type [<Measure>] Ign

type Registers = {
    IpBind: int<Reg>
    Ip: int<Reg>
    R0: int<Val>
    R1: int<Val>
    R2: int<Val>
    R3: int<Val>
    R4: int<Val>
    R5: int<Val>
}
with
    override x.ToString () = sprintf "[%d, %d, %d, %d, %d, %d]" x.R0 x.R1 x.R2 x.R3 x.R4 x.R5
    static member New ip = { IpBind = ip; Ip = 0<Reg>; R0 = 0<Val>; R1 = 0<Val>; R2 = 0<Val>; R3 = 0<Val>; R4 = 0<Val>; R5 = 0<Val> }
    static member LoadIp (x: Registers) =
        match x.IpBind with
        | 0<Reg> -> { x with R0 = x.Ip * 1<Val/Reg> }
        | 1<Reg> -> { x with R1 = x.Ip * 1<Val/Reg> }
        | 2<Reg> -> { x with R2 = x.Ip * 1<Val/Reg> }
        | 3<Reg> -> { x with R3 = x.Ip * 1<Val/Reg> }
        | 4<Reg> -> { x with R4 = x.Ip * 1<Val/Reg> }
        | 5<Reg> -> { x with R5 = x.Ip * 1<Val/Reg> }
        | _ -> impossible ()
    static member SaveIp (x: Registers) =
        match x.IpBind with
        | 0<Reg> -> { x with Ip = x.R0 * 1<Reg/Val> }
        | 1<Reg> -> { x with Ip = x.R1 * 1<Reg/Val> }
        | 2<Reg> -> { x with Ip = x.R2 * 1<Reg/Val> }
        | 3<Reg> -> { x with Ip = x.R3 * 1<Reg/Val> }
        | 4<Reg> -> { x with Ip = x.R4 * 1<Reg/Val> }
        | 5<Reg> -> { x with Ip = x.R5 * 1<Reg/Val> }
        | _ -> impossible ()
    static member IncIp (x: Registers) = { x with Ip = x.Ip + 1<Reg> }
    member x.Set (i: int<Reg>) (value: int<Val>) = 
        match i with
        | 0<Reg> -> { x with R0 = value }
        | 1<Reg> -> { x with R1 = value }
        | 2<Reg> -> { x with R2 = value }
        | 3<Reg> -> { x with R3 = value }
        | 4<Reg> -> { x with R4 = value }
        | 5<Reg> -> { x with R5 = value }
        | _ -> impossible ()
    member x.Get (i: int<Reg>) = 
        match i with
        | 0<Reg> -> x.R0
        | 1<Reg> -> x.R1
        | 2<Reg> -> x.R2
        | 3<Reg> -> x.R3
        | 4<Reg> -> x.R4
        | 5<Reg> -> x.R5
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
    override x.ToString () =
        match x with
        | Addr (a, b, c) -> sprintf "addr %d %d %d" a b c
        | Addi (a, b, c) -> sprintf "addi %d %d %d" a b c
        | Mulr (a, b, c) -> sprintf "mulr %d %d %d" a b c
        | Muli (a, b, c) -> sprintf "muli %d %d %d" a b c
        | Banr (a, b, c) -> sprintf "banr %d %d %d" a b c
        | Bani (a, b, c) -> sprintf "bani %d %d %d" a b c
        | Borr (a, b, c) -> sprintf "borr %d %d %d" a b c
        | Bori (a, b, c) -> sprintf "bori %d %d %d" a b c
        | Setr (a, b, c) -> sprintf "setr %d %d %d" a b c
        | Seti (a, b, c) -> sprintf "seti %d %d %d" a b c
        | Gtir (a, b, c) -> sprintf "gtir %d %d %d" a b c
        | Gtri (a, b, c) -> sprintf "gtri %d %d %d" a b c
        | Gtrr (a, b, c) -> sprintf "gtrr %d %d %d" a b c
        | Eqir (a, b, c) -> sprintf "eqir %d %d %d" a b c
        | Eqri (a, b, c) -> sprintf "eqri %d %d %d" a b c
        | Eqrr (a, b, c) -> sprintf "eqrr %d %d %d" a b c

let apply op (regs: Registers) =
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

module Parser =
    open FParsec
    open Util.Parser

    let pint32ws = pint32 .>> ws
    let parseTriple = pipe3 pint32ws pint32ws pint32ws (fun a b c -> (a, b, c))
    let parseAddr = str "addr " >>. parseTriple |>> (fun (a, b, c) -> Addr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>))
    let parseAddi = str "addi " >>. parseTriple |>> (fun (a, b, c) -> Addi (a * 1<Reg>, b * 1<Val>, c * 1<Reg>))
    let parseMulr = str "mulr " >>. parseTriple |>> (fun (a, b, c) -> Mulr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>))
    let parseMuli = str "muli " >>. parseTriple |>> (fun (a, b, c) -> Muli (a * 1<Reg>, b * 1<Val>, c * 1<Reg>))
    let parseBanr = str "banr " >>. parseTriple |>> (fun (a, b, c) -> Banr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>))
    let parseBani = str "bani " >>. parseTriple |>> (fun (a, b, c) -> Bani (a * 1<Reg>, b * 1<Val>, c * 1<Reg>))
    let parseBorr = str "borr " >>. parseTriple |>> (fun (a, b, c) -> Borr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>))
    let parseBori = str "bori " >>. parseTriple |>> (fun (a, b, c) -> Bori (a * 1<Reg>, b * 1<Val>, c * 1<Reg>))
    let parseSetr = str "setr " >>. parseTriple |>> (fun (a, b, c) -> Setr (a * 1<Reg>, b * 1<Ign>, c * 1<Reg>))
    let parseSeti = str "seti " >>. parseTriple |>> (fun (a, b, c) -> Seti (a * 1<Val>, b * 1<Ign>, c * 1<Reg>))
    let parseGtir = str "gtir " >>. parseTriple |>> (fun (a, b, c) -> Gtir (a * 1<Val>, b * 1<Reg>, c * 1<Reg>))
    let parseGtri = str "gtir " >>. parseTriple |>> (fun (a, b, c) -> Gtri (a * 1<Reg>, b * 1<Val>, c * 1<Reg>))
    let parseGtrr = str "gtrr " >>. parseTriple |>> (fun (a, b, c) -> Gtrr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>))
    let parseEqir = str "eqir " >>. parseTriple |>> (fun (a, b, c) -> Eqir (a * 1<Val>, b * 1<Reg>, c * 1<Reg>))
    let parseEqri = str "eqri " >>. parseTriple |>> (fun (a, b, c) -> Eqri (a * 1<Reg>, b * 1<Val>, c * 1<Reg>))
    let parseEqrr = str "eqrr " >>. parseTriple |>> (fun (a, b, c) -> Eqrr (a * 1<Reg>, b * 1<Reg>, c * 1<Reg>))

    let parseOp = 
        choice [
            attempt parseAddr
            attempt parseAddi
            attempt parseMulr
            attempt parseMuli
            attempt parseBanr
            attempt parseBani
            attempt parseBorr
            attempt parseBori
            attempt parseSetr
            attempt parseSeti
            attempt parseGtir
            attempt parseGtri
            attempt parseGtrr
            attempt parseEqir
            attempt parseEqri
            attempt parseEqrr
        ]

    let parseIp = str "#ip " >>. pint32 |>> fun ip -> ip * 1<Reg>

    let readOp input = Parser.readOrThrow (parseOp .>> eof) input
    let readIp input = Parser.readOrThrow (parseIp .>> eof) input

let part1 () =
    let lines = 
        readLines "Day19.input"
        |> List.ofSeq
    let ip, ops = 
        match lines with
        | ip :: ops ->
            let ip = Parser.readIp ip
            let ops = 
                ops
                |> List.map Parser.readOp 
                |> Array.ofList
            ip, ops
        | _ -> impossible ()
    let regs = Registers.New ip
    let rec loop regs =
        let oOp = Array.tryItem (regs.Ip / 1<Reg>) ops
        match oOp with
        | None -> regs
        | Some op ->
            Registers.SaveIp regs
            |> apply op
            |> Registers.LoadIp
            |> Registers.IncIp
            |> loop 
            // let a = Registers.LoadIp regs
            // let b = apply op a
            // let c = Registers.SaveIp b
            // let d = Registers.IncIp c
            // printfn "ip=%d %O %O %O" regs.Ip a op b
            // loop d
    let final = loop regs
    regs.R0
