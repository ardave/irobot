module PacketGroupParsing

open BitStuff
open iRobot
open Sensors

let private parseBumpsWheeldrops b =
    {
        BumpRight      = b |> isBitSet 0
        BumpLeft       = b |> isBitSet 1
        WheelDropRight = b |> isBitSet 2
        WheelDropLeft  = b |> isBitSet 3
    }

let private parseWall = firstBitOfByteToBool

let inline hiLoBytetoInt ba = 
    let len = ba |> Array.length
    match len with
    | 2 ->
        let highByte = int ba.[1]
        let lowByte  = int ba.[0]
        (highByte <<< 8) + lowByte
    | _ -> failwithf "Expected 2 byte array, but length was %i" len


let parsePacketGroup byteList = function
    | Group100 ->
        let len = byteList |> List.length
        match len with
        | 80 ->
            let ba = byteList |> List.rev |> List.toArray
            Ok({
                    defaultSensorData with
                        BumpDrop = Some(parseBumpsWheeldrops ba.[0])
                        Wall = Some(parseWall ba.[1])
                        WallSignal = Some(hiLoBytetoInt ba.[26..27])
                        CliffLeftSignal = Some(hiLoBytetoInt ba.[28..29])
                        CliffFrontLeftSignal = Some(hiLoBytetoInt ba.[30..31])
                        CliffFrontRightSignal = Some(hiLoBytetoInt ba.[32..33])
                        CliffRightSignal = Some(hiLoBytetoInt ba.[34..35])
                        BatteryCharge = Some((hiLoBytetoInt ba.[22..23]) * 1<mAh>)
                        LightBumpLeftSignal          = Some(hiLoBytetoInt ba.[57..58])
                        LightBumpFrontLeftSignal           = Some(hiLoBytetoInt ba.[59..60])
                        LightBumpCenterLeftSignal          = Some(hiLoBytetoInt ba.[61..62])
                        LightBumpCenterRightSignal   = Some(hiLoBytetoInt ba.[63..64])
                        LightBumpFrontRightSignal          = Some(hiLoBytetoInt ba.[65..66])
                        LightBumpRightSignal         = Some(hiLoBytetoInt ba.[67..68])
                })
        | _ -> Error(sprintf "Packet group 100 must be 80 bytes in length, but was %i" len)