module MessageAgent

open System
open iRobot

type Message = 
| UserKeyPress of ConsoleKeyInfo
| ByteReceivedFromRobot of byte

let createMessageAgent(writeBytes) =
    // One bit of mutable state at the top of the application,
    // that I suppose I've got to hold somewhere:
    let mutable roomba = Roomba.createDefault writeBytes

    MailboxProcessor.Start(fun inbox->
    let rec messageLoop() = async {
        let! msg = inbox.Receive()
        
        match msg with
        | UserKeyPress keyInfo ->
            printfn "Processing: %c" <| keyInfo.KeyChar
            match keyInfo.Key with
            | ConsoleKey.M          -> roomba <- Roomba.readSensors roomba
            | ConsoleKey.S          -> roomba <- Roomba.start       roomba
            | ConsoleKey.F          -> roomba <- Roomba.safe        roomba
            | ConsoleKey.Spacebar   -> roomba <- Roomba.stop        roomba
            | ConsoleKey.R          -> roomba <- Roomba.reset       roomba
            | ConsoleKey.X          -> roomba <- Roomba.beginStreaming roomba
            | ConsoleKey.P          -> roomba <- Roomba.drive    0<mm/second> Actuation.Straight roomba
            | ConsoleKey.UpArrow    -> roomba <- Roomba.drive  100<mm/second> Actuation.Straight roomba
            | ConsoleKey.DownArrow  -> roomba <- Roomba.drive -100<mm/second> Actuation.Straight roomba
            | ConsoleKey.LeftArrow  -> roomba <- Roomba.drive  100<mm/second> Actuation.TurnInPlaceCounterclockwise roomba
            | ConsoleKey.RightArrow -> roomba <- Roomba.drive  100<mm/second> Actuation.TurnInPlaceClockwise roomba
            | _ -> ()
        | ByteReceivedFromRobot b ->
            roomba <- Roomba.processByte b roomba
        
        return! messageLoop()
        }
    messageLoop()
    )