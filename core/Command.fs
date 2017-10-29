namespace iRobot

open Communicator
open OperatingMode

// Pg 6 at the top suggests that this type of command might be 
// different from the CleaningModeCommand
type Command =
| Start of CommandData
| Reset of CommandData
| Stop of CommandData
| Baud of CommandData

module CommandConstructors =
    let createStart = Start <| { OpCode = 128uy; DataBytes = Array.empty }
    let createReset = Reset <| { OpCode =   7uy; DataBytes = Array.empty }
    let createStop  = Stop  <| { OpCode = 173uy; DataBytes = Array.empty }
    let createBaud (baudRate:BaudRate) = Baud <| { OpCode = 129uy; DataBytes = [|byte baudRate|] }

    let sendCommand roomba command =
        let xmitCommand commandData newMode = 
            commandData |> transmitCommandData
            { roomba with OperatingMode = newMode}
        match roomba.OperatingMode with 
        | Off ->
            match command with
            | Start commandData -> xmitCommand commandData Passive
            | Reset commandData -> xmitCommand commandData Off
            | Stop  _ -> failwith "illegal (but does it matter?)"
            | Baud  _ -> failwith "illegal (but does it matter?)"
        | Passive ->
            match command with
            | Start commandData -> xmitCommand commandData Passive
            | Reset commandData -> xmitCommand commandData Off
            | Stop  commandData -> xmitCommand commandData Off
            | Baud  commandData -> xmitCommand commandData roomba.OperatingMode
        | Safe _ ->
            match command with
            | Start commandData -> xmitCommand commandData Passive
            | Reset commandData -> xmitCommand commandData Off
            | Stop  commandData -> xmitCommand commandData Off
            | Baud  commandData -> xmitCommand commandData roomba.OperatingMode
        | Full _ ->
            match command with
            | Start commandData -> xmitCommand commandData Passive
            | Reset commandData -> xmitCommand commandData Off
            | Stop  commandData -> xmitCommand commandData Off
            | Baud  commandData -> xmitCommand commandData roomba.OperatingMode
