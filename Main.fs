module Main

[<EntryPoint>]
let main argv =
    let result = Day10.all ()
    printfn "%O" result
    0