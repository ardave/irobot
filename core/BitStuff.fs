module BitStuff

open System

let intToTwosComplementBytes i =
    let input = 
        if i >= 0 then
            i
        else
            i
            |> abs
            |> (~~~)
            |> (+) 1
    printfn "Available bytes for i (%i) -> input (%i)" i input

    BitConverter.GetBytes(input)
    |> Array.iteri (fun idx elem -> printfn "%i: %i" idx elem)

    [| BitConverter.GetBytes(input).[1]; BitConverter.GetBytes(input).[0] |]