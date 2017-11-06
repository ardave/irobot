module PacketGroupParsingTests

open Xunit
open FsUnit.Xunit

open Sensors

[<Fact>]
let ``Should be able to parse a direct Light Bump Sensors Stream Packet``() =
    let resultf = 
        [19; 18; 46; 0; 0; 47; 0; 0; 48; 0; 0; 49; 0; 0; 50; 0; 0; 51; 0; 0; 184]
        |> List.rev
        |> List.map byte
        |> PacketGroupParsing.parsePacketGroup
    let result = resultf LightBumpSensors
    
    match result with 
    | Error e -> failwith e
    | Ok sensorData ->
        sensorData.LightBumpLeftSignal.IsSome        |> should be True
        sensorData.LightBumpFrontLeftSignal.IsSome   |> should be True
        sensorData.LightBumpCenterLeftSignal.IsSome  |> should be True
        sensorData.LightBumpCenterRightSignal.IsSome |> should be True
        sensorData.LightBumpFrontRightSignal.IsSome  |> should be True
        sensorData.LightBumpRightSignal.IsSome       |> should be True
