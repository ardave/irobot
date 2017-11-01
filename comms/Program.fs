﻿module iRobot.Comms

open System
open RJCP.IO.Ports

let realConnection port =
    let byteReceived = new Event<byte>()
    let inputBuffer = [|0uy|]
    let src = new SerialPortStream(port, ReceivedBytesThreshold = 1)
    src.Open()
    src.DataReceived.Add
        (fun a ->
            src.Read(inputBuffer, 0, 1) |> ignore
            byteReceived.Trigger inputBuffer.[0])
    let writeBytes (byteArray: byte array) =
        src.Write(byteArray, 0, byteArray.Length)

    byteReceived.Publish.Add, writeBytes, Some(src :> IDisposable)

let fakeConnection() =
    let writeBytes _ = ()
    Event<byte>().Publish.Add, writeBytes, None

type ConnectionType =
| Real of string
| Fake

let byteReceived connectionType =
    match connectionType with 
    | Real dev -> realConnection dev
    | Fake -> fakeConnection()

[<EntryPoint>]
let main argv = 
    0