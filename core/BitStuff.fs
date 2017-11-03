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
    [| BitConverter.GetBytes(input).[1]; BitConverter.GetBytes(input).[0] |]