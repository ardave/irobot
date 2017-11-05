namespace iRobot

open System
open System.Collections.Generic
open System.Diagnostics
open Sensors
open OperatingMode

type ReceivedByte = {
    Byte       : byte
    ReceivedAt : TimeSpan
}

type PacketExpectation = {
    BytesReceived : byte list
    BytesExpected : int
    PacketGroup   : PacketGroup
    Stopwatch     : Stopwatch
}

type Roomba = {
    OperatingMode     : OperatingMode
    SendCommand       : CommandData -> unit
    ReceivedByteLog   : Queue<ReceivedByte>
    PacketExpectation : PacketExpectation option
    Started           : bool
}

module Roomba =
    let private startTime = DateTime.Now 

    let createDefault writeBytes =
        { 
            OperatingMode     = Off
            SendCommand       = writeBytes
            ReceivedByteLog   = Queue<ReceivedByte>()
            PacketExpectation = None
            Started           = false
        }
    let private sendCommand command dataBytes roomba =
        {
            OpCode    = CommandConstructors.getCommandOpCode command
            DataBytes = dataBytes
        }
        |> roomba.SendCommand
        roomba

    let private sendModeCommand roomba = function
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
        sendModeCommand roomba OperatingMode.createSafe
        { roomba with OperatingMode = OperatingMode.createSafe }

    let drive velocity radius roomba =
        roomba.SendCommand <| Actuation.createDriveCommand velocity radius
        roomba

    let readSensors roomba = 
        roomba.SendCommand <| { OpCode = 142uy; DataBytes = [|100uy|] }
        let expectation = Some { BytesReceived = []; BytesExpected = 80; PacketGroup = Group100; Stopwatch = Stopwatch.StartNew() }
        { roomba with PacketExpectation = expectation }

    let private logByte b roomba =
        roomba.ReceivedByteLog.Enqueue({ Byte = b; ReceivedAt = DateTime.Now - startTime })
        if roomba.ReceivedByteLog.Count > 16483 then
            for _ in [0..100] do 
                roomba.ReceivedByteLog.Dequeue() |> ignore
    let processByte b roomba =
        logByte b roomba
        let updatedExpectation =
            match roomba.PacketExpectation with
            | Some e ->
                match e.BytesExpected with 
                | 1 ->
                    let sdResult = PacketGroupParsing.parsePacketGroup (b::e.BytesReceived) e.PacketGroup
                    printfn "Retrieved sensor data in %i ms." e.Stopwatch.ElapsedMilliseconds
                    match sdResult with 
                    | Ok sensorData -> SensorDataPrinting.print sensorData
                    | Error msg -> printfn "%s" msg
                    None
                | _ ->
                    Some({ e with 
                            BytesExpected = e.BytesExpected - 1
                            BytesReceived = b::e.BytesReceived
                        })
            | None -> 
                printf "%c" <| char b
                None
        { roomba with PacketExpectation = updatedExpectation }
