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

type PreludeByte =
| Received
| NotReceived

type DataAcquisitionMode = 
| Streaming of PreludeByte
| OneTime

type PacketExpectation = {
    BytesReceived       : byte list
    TotalBytesExpected  : int
    BytesRemaining      : int
    PacketGroup         : PacketGroup
    Stopwatch           : Stopwatch
    DataAcquisitionMode : DataAcquisitionMode
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
    let private sendGettingStartedCommand command dataBytes roomba =
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

    let start roomba = sendGettingStartedCommand Start Array.empty roomba
    let stop  roomba = sendGettingStartedCommand Stop  Array.empty roomba
    let reset roomba = sendGettingStartedCommand Reset Array.empty roomba

    let setBaudRate (baudRate:BaudRate) roomba =
        sendGettingStartedCommand Baud [|byte baudRate|] roomba

    let safe roomba =
        sendModeCommand roomba OperatingMode.createSafe
        { roomba with OperatingMode = OperatingMode.createSafe }

    let drive velocity radius roomba =
        roomba.SendCommand <| Actuation.createDriveCommand velocity radius
        roomba

    let createPacketExpectation b pg m = 
        {
            BytesReceived       = []
            BytesRemaining      = b
            TotalBytesExpected  = b
            PacketGroup         = pg
            Stopwatch           = Stopwatch.StartNew()
            DataAcquisitionMode = m
        }
    let readSensors roomba = 
        roomba.SendCommand <| { OpCode = 142uy; DataBytes = [|100uy|] }
        { roomba with PacketExpectation = Some(createPacketExpectation 80 Group100 OneTime) }

    let private logByte b roomba =
        roomba.ReceivedByteLog.Enqueue({ Byte = b; ReceivedAt = DateTime.Now - startTime })
        if roomba.ReceivedByteLog.Count > 16483 then
            for _ in [0..100] do 
                roomba.ReceivedByteLog.Dequeue() |> ignore

    let beginStreaming roomba =
        let requestedPackets = [| 46uy; 47uy; 48uy; 49uy; 50uy; 51uy |]
        let numberOfPackets = [| requestedPackets |> Array.length |> byte |]
        let commandData = { OpCode = 148uy; DataBytes = Array.append numberOfPackets requestedPackets }
        roomba.SendCommand commandData
        { roomba with PacketExpectation = Some(createPacketExpectation 21 LightBumpSensors (Streaming(NotReceived))) }

    let processByte b roomba =
        logByte b roomba
        let updatedExpectation =
            match roomba.PacketExpectation with
            | Some e ->
                match e.BytesRemaining with 
                | 1 ->
                    let sensorDataResult = PacketGroupParsing.parsePacketGroup (b::e.BytesReceived) e.PacketGroup
                    printfn "Retrieved sensor data in %i ms." e.Stopwatch.ElapsedMilliseconds
                    match sensorDataResult with 
                    | Ok sensorData -> SensorDataPrinting.print sensorData
                    | Error msg -> printfn "%s" msg
                    match e.DataAcquisitionMode with 
                    | OneTime     -> None
                    | Streaming _ -> Some(createPacketExpectation e.TotalBytesExpected e.PacketGroup (Streaming(NotReceived)))
                | _ ->
                    match e.DataAcquisitionMode with 
                    | Streaming preludeByte ->
                        match preludeByte, (int b) with 
                        | NotReceived, 19 ->
                            Some({ e with 
                                    BytesRemaining = e.BytesRemaining - 1
                                    BytesReceived = b::e.BytesReceived
                                    DataAcquisitionMode = Streaming Received
                            })
                        | NotReceived, _  -> Some e
                        | Received, _     ->
                            Some({ e with 
                                    BytesRemaining = e.BytesRemaining - 1
                                    BytesReceived = b::e.BytesReceived
                            })
                    | OneTime ->
                        Some({ e with 
                                BytesRemaining = e.BytesRemaining - 1
                                BytesReceived = b::e.BytesReceived
                            })
            | None -> 
                // printf "%c" <| char b
                printf "%i " b
                None
        { roomba with PacketExpectation = updatedExpectation }
