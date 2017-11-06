module SensorDataPrinting

open Sensors

let printBumpDrop bd = 
    printfn "Bump Left        : %b" bd.BumpLeft
    printfn "Bump Right       : %b" bd.BumpRight
    printfn "Wheel Drop Left  : %b" bd.WheelDropLeft
    printfn "Wheel Drop Right : %b" bd.WheelDropRight

let printWall w = printfn "Wall: %b" w
let printWallSignal s = printfn "Wall Signal: %i" s
let printCliffLeftSignal s = printfn "Cliff Left: %i" s
let printCliffFrontLeftSignal s = printfn "Cliff Front Left: %i" s
let printCliffFrontRightSignal s = printfn "Cliff Front Right: %i" s
let printCliffRightSignal s = printfn "Cliff Right: %i" s

let printLightBumpLeftSignal s = printfn "Light Bump Left: %i" s
let printLightBumpFrontLeftSignal s = printfn "Light Bump Front Left: %i" s
let printLightBumpCenterLeftSignal s = printfn "Light Bump Center Left: %i" s
let printLightBumpCenterRightSignal s = printfn "Light Bump Center Right: %i" s
let printLightBumpFrontRightSignal s = printfn "Light Bump Front Right: %i" s
let printLightBumpRightSignal s = printfn "Light Bump Right: %i" s
let printBatteryCharge s = printfn "Battery Charge: %A" s

let print sensorData = 
    // printfn "******************************"
    sensorData.BumpDrop |> Option.iter printBumpDrop
    sensorData.Wall |> Option.iter printWall
    sensorData.WallSignal |> Option.iter printWallSignal
    sensorData.CliffLeftSignal |> Option.iter printCliffLeftSignal
    sensorData.CliffFrontLeftSignal |> Option.iter printCliffFrontLeftSignal
    sensorData.CliffFrontRightSignal |> Option.iter printCliffFrontRightSignal
    sensorData.CliffRightSignal |> Option.iter printCliffRightSignal
    sensorData.BatteryCharge |> Option.iter printBatteryCharge
    sensorData.LightBumpLeftSignal |> Option.iter printLightBumpLeftSignal
    sensorData.LightBumpFrontLeftSignal |> Option.iter printLightBumpFrontLeftSignal
    sensorData.LightBumpCenterLeftSignal |> Option.iter printLightBumpCenterLeftSignal
    sensorData.LightBumpCenterRightSignal |> Option.iter printLightBumpCenterLeftSignal
    sensorData.LightBumpFrontRightSignal |> Option.iter printLightBumpFrontRightSignal
    sensorData.LightBumpRightSignal |> Option.iter printLightBumpRightSignal

