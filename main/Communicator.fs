module Communicator 

open iRobot

let transmitCommandData commandData =
    let dataSequence = Array.append [|commandData.OpCode|] commandData.DataBytes
    printfn "Transmitting CommandData: %A" dataSequence