module PacketGroupParsing

open BitStuff
open Sensors

let private parseBumpsWheeldrops b =
    {
        BumpRight      = b |> isBitSet 0
        BumpLeft       = b |> isBitSet 1
        WheelDropRight = b |> isBitSet 2
        WheelDropLeft  = b |> isBitSet 3
    }

let private parseWall = firstBitOfByteToBool

let parsePacketGroup bytes = function
    | Group100 ->
        let len = bytes |> List.length
        match len with
        | 80 ->
            Ok({
                    defaultSensorData with
                        BumpDrop = Some(parseBumpsWheeldrops bytes.[0])
                        Wall = Some(parseWall bytes.[1])
                })
        | _ -> Error(sprintf "Packet group 100 must be 80 bytes in length, but was %i" len)