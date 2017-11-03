namespace iRobot

open System
open System.Collections.Generic
open OperatingMode

type ReceivedByte = {
    Byte       : byte
    ReceivedAt : TimeSpan
}

type Roomba = {
    OperatingMode   : OperatingMode
    SendCommand     : CommandData -> unit
    ReceivedByteLog : Queue<ReceivedByte>
}

module Roomba =
    let private startTime = DateTime.Now 

    let createDefault writeBytes=
        { 
            OperatingMode   = Off
            SendCommand     = writeBytes
            ReceivedByteLog = Queue<ReceivedByte>()
        }

    let private sendCommand command dataBytes roomba =
        {
            OpCode    = CommandConstructors.getCommandOpCode command
            DataBytes = dataBytes
        }
        |> roomba.SendCommand
        roomba

    let private sendModeCommand modeCommand roomba = 
        modeCommand
        |> roomba.SendCommand

    let start roomba = sendCommand Start Array.empty roomba
    let stop  roomba = sendCommand Stop  Array.empty roomba
    let reset roomba = sendCommand Reset Array.empty roomba

    let setBaudRate (baudRate:BaudRate) roomba =
        sendCommand Baud [|byte baudRate|] roomba

    let safe roomba =
        sendModeCommand OperatingMode.createSafe roomba
        roomba

    let moveForward velocity roomba =
        // Actuation.getDriveCommand velocity Actuation.Straight
        {OpCode = 137uy; DataBytes =  [|255uy; 56uy; 1uy; 244uy|] }
        |> roomba.SendCommand 
        roomba

    let processByte b roomba =
        roomba.ReceivedByteLog.Enqueue({ Byte = b; ReceivedAt = DateTime.Now - startTime })
        if roomba.ReceivedByteLog.Count > 16483 then
            for _ in [0..100] do 
                roomba.ReceivedByteLog.Dequeue() |> ignore
        roomba
