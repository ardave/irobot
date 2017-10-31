open System
open MessageAgent
open iRobot.Comms

[<EntryPoint>]
let main argv = 
    let messageAgent = createMessageAgent()

    (messageAgent.Post << ByteReceivedFromRobot)
    |> byteReceived (Real("/dev/ttyUSB0"))
   
    let rec monitorKeyboardInput() =
        let keyInfo = Console.ReadKey()
        match keyInfo.Key with
        | ConsoleKey.Q -> printfn "Cancel key pressed."
        | _ -> 
            messageAgent.Post <| UserKeyPress(keyInfo)
            monitorKeyboardInput()
    monitorKeyboardInput()
    0