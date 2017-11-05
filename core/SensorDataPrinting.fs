module SensorDataPrinting

open Sensors

let printBumpDrop bd = 
    printfn "Bump Left        : %b" bd.BumpLeft
    printfn "Bump Right       : %b" bd.BumpRight
    printfn "Wheel Drop Left  : %b" bd.WheelDropLeft
    printfn "Wheel Drop Right : %b" bd.WheelDropRight

let printWall w =
    printfn "Wall: %b" w

let print sensorData = 
    sensorData.BumpDrop |> Option.iter printBumpDrop
    sensorData.Wall |> Option.iter printWall
    