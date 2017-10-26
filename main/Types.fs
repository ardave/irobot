module Types

open Communicator
open iRobot

let getOperatingModeCommandData operatingMode = 
    match operatingMode with 
    | Safe -> { OpCode = 131uy; DataBytes = Array.empty }
    | Full -> { OpCode = 132uy; DataBytes = Array.empty }
    | _    -> failwith "not covered by the manual!"

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
    | Safe ->
        match command with
        | Start commandData -> xmitCommand commandData Passive
        | Reset commandData -> xmitCommand commandData Off
        | Stop  commandData -> xmitCommand commandData Off
        | Baud  commandData -> xmitCommand commandData roomba.OperatingMode
    | Full ->
        match command with
        | Start commandData -> xmitCommand commandData Passive
        | Reset commandData -> xmitCommand commandData Off
        | Stop  commandData -> xmitCommand commandData Off
        | Baud  commandData -> xmitCommand commandData roomba.OperatingMode

let sendModeCommand roomba command = 
    match command with 
    | _ -> { roomba with OperatingMode = Passive }

