open System
open MessageAgent
open iRobot.Comms

[<EntryPoint>]
let main argv = 
    let messageAgent = createMessageAgent()

    let br, writeBytes, disposableOpt = byteReceived (Real("/dev/ttyUSB0"))
    br (messageAgent.Post << ByteReceivedFromRobot)
    writeBytes[|128uy|]
   
    let rec monitorKeyboardInput() =
        let keyInfo = Console.ReadKey()
        match keyInfo.Key with
        | ConsoleKey.Q -> printfn "Cancel key pressed."
        | ConsoleKey.DownArrow -> writeBytes [|137uy; 0uy; 50uy; 0uy; 0uy|]
        | _ -> 
            messageAgent.Post <| UserKeyPress(keyInfo)
            monitorKeyboardInput()
    monitorKeyboardInput()
    match disposableOpt with | Some d -> d.Dispose() | None -> ()
    0