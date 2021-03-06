﻿open System
open MessageAgent
open iRobot
open iRobot.Comms

[<EntryPoint>]
let main _ =
    printfn "Starting ..."
    let byteReceived, writeBytes, disposableOpt = createConnection (Real("/dev/ttyUSB0"))
    let messageAgent = createMessageAgent(writeBytes)
    messageAgent.Error.Add
        (fun (exn:Exception) -> printfn "Unhandled Exception:\n%A" exn)

    try
        byteReceived (messageAgent.Post << ByteReceivedFromRobot)

        let rec monitorKeyboardInput() =
            let keyInfo = Console.ReadKey()
            match keyInfo.Key with
            | ConsoleKey.Q ->
                printfn "Cancel key pressed."
            | _ -> 
                messageAgent.Post <| UserKeyPress(keyInfo)
                monitorKeyboardInput()

        monitorKeyboardInput()
    finally
        match disposableOpt with | Some d -> d.Dispose() | None -> ()
    
    0
