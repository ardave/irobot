[<EntryPoint>]
let main argv =
    let bytes = BitStuff.intToTwosComplementBytes 25
    printfn "Bytes: %A" bytes
    0
