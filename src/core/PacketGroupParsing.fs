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

let packetLengths =
    [   7, 1; 8, 1;   9, 1; 10, 1; 11, 1; 12, 1; 13, 1; 14, 1; 15, 1; 16, 1; 17, 1; 18, 1; 19, 2; 20, 2;
       21, 1; 22, 2; 23, 2; 24, 1; 25, 2; 26, 2; 27, 2; 28, 2; 29, 2; 30, 2; 31, 2; 32, 1; 33, 2; 34, 1;
       35, 1; 36, 1; 37, 1; 38, 1; 39, 2; 40, 2; 41, 2; 42, 2; 43, 2; 44, 2; 45, 1; 46, 2; 47, 2; 48, 2; 
       49, 2; 50, 2; 51, 2; 52, 1; 53, 1; 54, 2; 55, 2; 56, 2; 57, 2; 51, 1;
    ]
    |> Map.ofList

type StreamFoldState = 
| SeekingPacketNumber
| GettingBytes of int

let fstOf3 (x, _, _) = x

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
    | LightBumpSensors ->
        let len = byteList |> List.length
        match len < 5 with // prelude byte + stream length byte + at least 1 packet id + at least one packet byte + checksum byte
        | true -> Error(sprintf "Need at least 5 bytes to make valid stream, but was %i" len)
        | false ->
            // [19; 18; 46; 0; 0; 47; 0; 0; 48; 0; 0; 49; 0; 0; 50; 0; 0; 51; 0; 0; 184]
            let dataByteCount = int byteList.[1]
            let sensorData =
                byteList 
                |> List.skip 2
                |> List.fold (fun acc elem -> acc) (defaultSensorData, SeekingPacketNumber, dataByteCount)
                |> fstOf3

            byteList
            |> List.map int
            |> List.rev
            |> printfn "The Bytes are %A "
            
            Ok sensorData