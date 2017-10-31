module iRobot.Comms

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
    byteReceived.Publish.Add

let fakeConnection() =
    Event<byte>().Publish.Add

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