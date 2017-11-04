namespace iRobot

open System
open System.Collections.Generic
open Actuation
open OperatingMode

type ReceivedByte = {
    Byte       : byte
    ReceivedAt : TimeSpan
}

type Roomba = {
    OperatingMode     : OperatingMode
    SendCommand       : CommandData -> unit
    ReceivedByteLog   : Queue<ReceivedByte>
    CurrentDriveState : int<velocity> * Radius
}

module Roomba =
    let private startTime = DateTime.Now 

    let createDefault writeBytes=
        { 
            OperatingMode     = Off
            SendCommand       = writeBytes
            ReceivedByteLog   = Queue<ReceivedByte>()
            CurrentDriveState = 0<velocity>, ArbitraryRadius(0<mm>)
        }

    let private sendCommand command dataBytes roomba =
        {
            OpCode    = CommandConstructors.getCommandOpCode command
            DataBytes = dataBytes
        }
        |> roomba.SendCommand
        roomba

    let private sendModeCommand modeCommand roomba = 
        match modeCommand with
        | Safe commandData | Full commandData -> 
            commandData |> roomba.SendCommand
        | _ -> 
            failwith "not implemented"

    let start roomba = sendCommand Start Array.empty roomba
    let stop  roomba = sendCommand Stop  Array.empty roomba
    let reset roomba = sendCommand Reset Array.empty roomba

    let setBaudRate (baudRate:BaudRate) roomba =
        sendCommand Baud [|byte baudRate|] roomba

    let safe roomba =
        sendModeCommand OperatingMode.createSafe roomba
        roomba

    let drive velocity radius roomba =
        let currentVelocity = fst roomba.CurrentDriveState
        let newVelocity = if currentVelocity > 0<velocity> then 0<velocity> else velocity
        roomba.SendCommand <| Actuation.createDriveCommand newVelocity radius
        { roomba with CurrentDriveState = newVelocity, radius }

    let processByte b roomba =
        roomba.ReceivedByteLog.Enqueue({ Byte = b; ReceivedAt = DateTime.Now - startTime })
        if roomba.ReceivedByteLog.Count > 16483 then
            for _ in [0..100] do 
                roomba.ReceivedByteLog.Dequeue() |> ignore
        roomba
