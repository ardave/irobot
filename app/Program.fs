open System
open MessageAgent
open iRobot.Comms

[<EntryPoint>]
let main argv = 
    let messageAgent = createMessageAgent()

    let br, writeBytes, disposableOpt = byteReceived (Real("/dev/ttyUSB0"))
    br (messageAgent.Post << ByteReceivedFromRobot)
    System.Threading.Thread.Sleep(TimeSpan.FromSeconds(1.))
    writeBytes[|128uy|]
   
    let rec monitorKeyboardInput() =
        let keyInfo = Console.ReadKey()
        match keyInfo.Key with
        | ConsoleKey.Q -> printfn "Cancel key pressed."
        | ConsoleKey.DownArrow -> 
            writeBytes [|137uy; 255uy; 56uy; 1uy; 244uy|]
            monitorKeyboardInput()
        | ConsoleKey.RightArrow ->
            writeBytes [|128uy|]
            monitorKeyboardInput()
        | ConsoleKey.LeftArrow ->
            writeBytes [|7uy|]
            monitorKeyboardInput()
        | _ -> 
            messageAgent.Post <| UserKeyPress(keyInfo)
            monitorKeyboardInput()
    monitorKeyboardInput()
    match disposableOpt with | Some d -> d.Dispose() | None -> ()
    0