module iRobot.Comms

open System
open RJCP.IO.Ports
open iRobot

let realConnection port =
    printfn "Opening connection to %s ..." port
    let byteReceived = new Event<byte>()
    let inputBuffer = [|0uy|]
    let src = new SerialPortStream(port, 115200, 8, Parity.None, StopBits.One, ReceivedBytesThreshold = 1)
    src.Open()
    src.DataReceived.Add
        (fun _ ->
            src.Read(inputBuffer, 0, 1) |> ignore
            byteReceived.Trigger inputBuffer.[0])
    let writeBytes commandData =
        let allBytes = Array.append [|commandData.OpCode|] commandData.DataBytes
        printfn "Writing bytes: %A" allBytes
        src.Write(allBytes, 0, commandData.DataBytes.Length)
        src.Flush()

    byteReceived.Publish.Add, writeBytes, Some(src :> IDisposable)

let fakeConnection() =
    let writeBytes _ = ()
    Event<byte>().Publish.Add, writeBytes, None

type ConnectionType =
| Real of string
| Fake

let createConnection connectionType =
    match connectionType with 
    | Real dev -> realConnection dev
    | Fake -> fakeConnection()

[<EntryPoint>]
let main argv = 
    0