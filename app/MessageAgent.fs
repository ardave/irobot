module MessageAgent

open System
open iRobot
open iRobot.Comms

type Message = 
| UserKeyPress of ConsoleKeyInfo
| ByteReceivedFromRobot of byte

let createMessageAgent(writeBytes) =
    let mutable roomba = Roomba.createDefault writeBytes

    MailboxProcessor.Start(fun inbox->
    let rec messageLoop() = async {
        let! msg = inbox.Receive()
        
        match msg with
        | UserKeyPress keyInfo ->
            printfn "Processing: %c" <| keyInfo.KeyChar
            match keyInfo.Key with
            | ConsoleKey.S        -> roomba <- Roomba.start roomba
            | ConsoleKey.Spacebar -> roomba <- Roomba.stop  roomba
            | ConsoleKey.R        -> roomba <- Roomba.reset roomba
            | ConsoleKey.UpArrow  -> roomba <- Roomba.moveForward 25<mm/second> roomba
            | _ -> ()
        | ByteReceivedFromRobot b ->
            printfn "%i received." b
            roomba <- Roomba.processByte b roomba
        
        return! messageLoop()
        }
    messageLoop()
    )