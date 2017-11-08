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

type StreamingStep =
| Initial
| PreludeByteReceived
| DataLengthByteReceived

type DataAcquisitionMode = 
| Streaming of StreamingStep
| OneTime

// For streaming data, we need to receive the first
// couple of bytes before we know how many more bytes 
// to expect:
type BytesRemaining = 
| Unknown
| BytesRemaining of int

type PacketExpectation = {
    BytesReceived       : byte list
    TotalBytesExpected  : BytesRemaining
    BytesRemaining      : BytesRemaining
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

    let private createPacketExpectation b pg m = 
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
        { roomba with PacketExpectation = Some(createPacketExpectation (BytesRemaining(80)) Group100 OneTime) }

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
        { roomba with PacketExpectation = Some(createPacketExpectation Unknown LightBumpSensors (Streaming(Initial))) }

    let private parsePacketGroup e b =
        let sw = Stopwatch.StartNew()
        let sensorDataResult = PacketGroupParsing.parsePacketGroup (b::e.BytesReceived) e.PacketGroup
        // printfn "Parsed packet group in %i ms." sw.ElapsedMilliseconds

        // printfn "%A" sensorDataResult
        
        match sensorDataResult with 
        | Ok sensorData -> 
            match e.DataAcquisitionMode with 
            | Streaming _ ->
                ()//printfn "Retrieved sensor data in %i ms." e.Stopwatch.ElapsedMilliseconds
            | OneTime ->
                SensorDataPrinting.print sensorData
        | Error msg     -> 
            printfn "%s" msg
        match e.DataAcquisitionMode with 
        | OneTime     -> None
        | Streaming _ -> Some(createPacketExpectation e.TotalBytesExpected e.PacketGroup (Streaming(Initial)))

    // I think the failwiths are exposing an inaccuracy in the shape of my ADTs:
    let private receiveIntermediateByte pe b =
        match pe.DataAcquisitionMode with 
        | Streaming streamingStep ->
            match streamingStep with 
            | Initial ->
                match int b with 
                | 19 ->
                    Some({ pe with 
                            BytesRemaining = Unknown
                            BytesReceived = b::pe.BytesReceived
                            DataAcquisitionMode = Streaming PreludeByteReceived
                    })
                | _ -> Some pe
            | PreludeByteReceived -> Some({ pe with 
                                                BytesRemaining = BytesRemaining(int b)
                                                BytesReceived = b::pe.BytesReceived
                                                DataAcquisitionMode = Streaming DataLengthByteReceived
                                     })
            | DataLengthByteReceived -> Some({ pe with 
                                                BytesRemaining = 
                                                    match pe.BytesRemaining with
                                                    | BytesRemaining br -> BytesRemaining(br - 1)
                                                    | Unknown           -> failwith "Bytes Remaining should never be 'Unknown' for a a stream in the DataLength streaming step."
                                                BytesReceived = b::pe.BytesReceived
                                                DataAcquisitionMode = Streaming DataLengthByteReceived
                                        })
        | OneTime ->
            Some({ pe with
                    BytesRemaining =
                        match pe.BytesRemaining with
                        | BytesRemaining br -> BytesRemaining(br - 1)
                        | Unknown           -> failwith "Bytes Remaining should never be 'Unknown' for a OneTime data acquisition"
                    BytesReceived = b::pe.BytesReceived
                })

        //     match preludeByte, (int b) with 
        //     | NotReceived, 19 ->
        //         Some({ pe with 
        //                 BytesRemaining = Unknown
        //                 BytesReceived = b::pe.BytesReceived
        //                 DataAcquisitionMode = Streaming Received
        //         })
        //     | NotReceived, _  -> Some pe
        //     | Received,    _  ->
        //         Some({ pe with
        //                 BytesRemaining =
        //                     match pe.BytesRemaining with
        //                     | BytesRemaining br -> BytesRemaining(br - 1)
        //                     | Unknown           -> Unknown
        //                 BytesReceived = b::pe.BytesReceived
        //         })
        // | OneTime ->
        //     Some({ pe with
        //             BytesRemaining =
        //                 match pe.BytesRemaining with
        //                 | BytesRemaining br -> BytesRemaining(br - 1)
        //                 | Unknown           -> failwith "Bytes Remaining should never be 'Unknown' for a OneTime data acquisition"
        //             BytesReceived = b::pe.BytesReceived
        //         })

    let private updateExpectation pe b =
        match pe.BytesRemaining with
        | Unknown ->
            receiveIntermediateByte pe b
        | BytesRemaining i ->
            match i with
            | 1 -> parsePacketGroup pe b
            | _ -> receiveIntermediateByte pe b

    let processByte b roomba =
        logByte b roomba
        let updatedExpectation =
            match roomba.PacketExpectation with
            | Some e ->
                updateExpectation e b
            | None -> 
                printf "%c" <| char b
                // printf "%i " b
                None
        { roomba with PacketExpectation = updatedExpectation }
