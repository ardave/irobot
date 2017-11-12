module PacketGroupParsing

open System
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
       49, 2; 50, 2; 51, 2; 52, 1; 53, 1; 54, 2; 55, 2; 56, 2; 57, 2; 58, 1;
    ]
    |> Map.ofList

let parsePacketGroup100 byteList =
    let len = byteList |> List.length
    match len with
    | 80 ->
        let ba = byteList |> List.rev |> List.toArray
        Ok({
                defaultSensorData with
                    BumpDrop                   = Some(parseBumpsWheeldrops ba.[0])
                    Wall                       = Some(parseWall ba.[1])
                    WallSignal                 = Some(hiLoBytetoInt ba.[26..27])
                    CliffLeftSignal            = Some(hiLoBytetoInt ba.[28..29])
                    CliffFrontLeftSignal       = Some(hiLoBytetoInt ba.[30..31])
                    CliffFrontRightSignal      = Some(hiLoBytetoInt ba.[32..33])
                    CliffRightSignal           = Some(hiLoBytetoInt ba.[34..35])
                    BatteryCharge              = Some((hiLoBytetoInt ba.[22..23]) * 1<mAh>)
                    LightBumpLeftSignal        = Some(hiLoBytetoInt ba.[57..58])
                    LightBumpFrontLeftSignal   = Some(hiLoBytetoInt ba.[59..60])
                    LightBumpCenterLeftSignal  = Some(hiLoBytetoInt ba.[61..62])
                    LightBumpCenterRightSignal = Some(hiLoBytetoInt ba.[63..64])
                    LightBumpFrontRightSignal  = Some(hiLoBytetoInt ba.[65..66])
                    LightBumpRightSignal       = Some(hiLoBytetoInt ba.[67..68])
            })
    | _ -> Error(sprintf "Packet group 100 must be 80 bytes in length, but was %i" len)

type PacketInfo = {
    PacketId       : int
    BytesRemaining : int
    Bytes          : byte list
} 

type StreamParsingState = 
| SeekingPacketNumber
| ParsingPacket of PacketInfo

let fstOf3 (x, _, _) = x

let addPacketToSensorData sensorData packetInfo =
    // printfn "---- Adding packet to sensor data: %A" packetInfo

    let bytes = packetInfo.Bytes |> List.rev |> List.toArray
    match packetInfo.PacketId with
    | 19 ->
        let additionalMMs = hiLoBytetoInt bytes * 1<mm>
        let newmms =
            match sensorData.Distance with
            | Some mms -> Some(mms + additionalMMs)
            | None     -> Some(additionalMMs)
        Ok({ sensorData with Distance = newmms })
    |20 ->
        let additionalDegrees = hiLoBytetoInt bytes * 1<degrees>
        let newmms =
            match sensorData.Angle with
            | Some degrees -> Some(degrees + additionalDegrees)
            | None         -> Some(additionalDegrees)
        Ok({ sensorData with Angle = newmms })
    | 21 -> Ok({ sensorData with ChargingState = Some(parseChargingState packetInfo.Bytes.[0])})
    | 25 -> Ok({ sensorData with BatteryCharge = Some((hiLoBytetoInt bytes) * 1<mAh>)})
    | 46 -> Ok({ sensorData with LightBumpLeftSignal        = Some(hiLoBytetoInt bytes) })
    | 47 -> Ok({ sensorData with LightBumpFrontLeftSignal   = Some(hiLoBytetoInt bytes) })
    | 48 -> Ok({ sensorData with LightBumpCenterLeftSignal  = Some(hiLoBytetoInt bytes) })
    | 49 -> Ok({ sensorData with LightBumpCenterRightSignal = Some(hiLoBytetoInt bytes) })
    | 50 -> Ok({ sensorData with LightBumpFrontRightSignal  = Some(hiLoBytetoInt bytes) })
    | 51 -> Ok({ sensorData with LightBumpRightSignal       = Some(hiLoBytetoInt bytes) })
    | _ -> 
        Error(sprintf "I don't know how to parse packet id %i" packetInfo.PacketId)


let parseLightBumpSensors byteList =
    let len = byteList |> List.length
    match len < 5 with // prelude byte + stream length byte + at least 1 packet id + at least one packet byte + checksum byte
    | true -> Error(sprintf "Need at least 5 bytes to make valid stream, but was %i" len)
    | false ->
        // [19; 18; 46; 0; 0; 47; 0; 0; 48; 0; 0; 49; 0; 0; 50; 0; 0; 51; 0; 0; 184]
        let rev = byteList |> List.rev
        // printfn "The bytes are: %A" rev
        let sensorData =
            rev 
            |> List.skip 2 // skip Packet ID and Byte count byte
            |> List.fold (fun (sensorDataOpt, parsingState, totalBytesRemaining) currentByte ->
                match sensorDataOpt with
                | Error _ -> (sensorDataOpt, parsingState, totalBytesRemaining - 1)
                | Ok sensorData ->
                    match parsingState with
                    | SeekingPacketNumber ->
                        match totalBytesRemaining with 
                        | 0 ->
                            (Ok(sensorData), parsingState, totalBytesRemaining)
                        | _ ->
                            match packetLengths.ContainsKey <| int currentByte with
                            | false ->
                                let msg = sprintf "Unknown length for packet ID: %i" <| int currentByte
                                (Error(msg), parsingState, totalBytesRemaining - 1)
                            | true -> 
                                let packetInfo = { PacketId = int currentByte; BytesRemaining = packetLengths.[int currentByte]; Bytes = [] }
                                (Ok(sensorData), ParsingPacket(packetInfo), totalBytesRemaining - 1)
                    | ParsingPacket packetInfo  ->
                        match packetInfo.BytesRemaining with 
                        | 1 -> 
                            let newSensorData = addPacketToSensorData sensorData { packetInfo with Bytes = currentByte::packetInfo.Bytes }
                            (newSensorData, SeekingPacketNumber, totalBytesRemaining - 1)
                        | _ ->
                            let newPacketInfo = { packetInfo with 
                                                        BytesRemaining = packetInfo.BytesRemaining - 1
                                                        Bytes = currentByte::packetInfo.Bytes
                                                }
                            (Ok(sensorData), ParsingPacket(newPacketInfo), totalBytesRemaining - 1)
                ) (Ok(defaultSensorData), SeekingPacketNumber, int rev.[1])
            |> fstOf3
        sensorData

let parsePacketGroup byteList packetGroup =
    let result = ResultBuilder()
    result {
        let! sensorData = match packetGroup with
                          | Group100         -> parsePacketGroup100 byteList
                          | LightBumpSensors -> parseLightBumpSensors byteList
        return { sensorData with Timestamp = DateTime.Now }
    }