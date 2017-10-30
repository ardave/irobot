module GroupPacketParser

open Sensors

let parseGroup0 ba =
    match ba |> Array.length with 
    | 26 ->
        let bumpsWheelDrops = parseBumpsWheeldrops ba.[0]
        { defaultSensorData with
            BumpDrop = Some bumpsWheelDrops
        }
    | x -> 
        failwithf "Packet group 0 should be contained in an array of length 26, but actual array was %i" x

