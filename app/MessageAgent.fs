module MessageAgent

open System

type Message = 
| UserKeyPress of ConsoleKeyInfo
| ByteReceivedFromRobot of byte

let createMessageAgent() = 
    MailboxProcessor.Start(fun inbox-> 

    // the message processing function
    let rec messageLoop() = async{
        
        // read a message
        let! msg = inbox.Receive()
        
        match msg with
        | UserKeyPress keyInfo -> printfn "Pressed: %s" <| keyInfo.ToString()
        | ByteReceivedFromRobot b -> printfn "%i received." b

        // loop to top
        return! messageLoop()  
        }
    messageLoop()
    )