namespace iRobot

module CleaningMode =

    open System
    open Communicator
    open OperatingMode

    type CleaningMode =
    | Spot       of CommandData
    | Clean      of CommandData
    | SeekDock   of CommandData
    | Power      of CommandData
    | Max        of CommandData
    | Schedule   of CommandData
    | SetDayTime of CommandData

    let createSpot       = { OpCode = 134uy; DataBytes = Array.empty }
    let createClean      = { OpCode = 135uy; DataBytes = Array.empty }
    let createSeekDock   = { OpCode = 143uy; DataBytes = Array.empty }
    let createPower      = { OpCode = 133uy; DataBytes = Array.empty }
    let createMax        = { OpCode = 136uy; DataBytes = Array.empty }
    let createSchedule   = { OpCode = 167uy; DataBytes = Array.empty }
    let createSetDayTime (dateTime:DateTime) =
        let data = 
            [| int dateTime.DayOfWeek; dateTime.Hour; dateTime.Minute |]
            |> Array.map byte
        { OpCode = 168uy; DataBytes = data }

    let sendCleaningModeCommand roomba (cleaningModeCommand:CleaningMode) =
        let xmitCommand newMode commandData =
            commandData |> transmitCommandData
            { roomba with OperatingMode = newMode }
        match roomba.OperatingMode with
        | Off ->
            match cleaningModeCommand with
            | Spot       _ -> failwith "illegal"
            | Clean      _ -> failwith "illegal"
            | SeekDock   _ -> failwith "illegal"
            | Power      _ -> failwith "illegal"
            | Max        _ -> failwith "illegal"
            | Schedule   _ -> failwith "illegal"
            | SetDayTime _ -> failwith "illegal"
        | Passive ->
            match cleaningModeCommand with
            | Spot       cd -> xmitCommand Passive cd
            | Clean      cd -> xmitCommand Passive cd
            | SeekDock   cd -> xmitCommand Passive cd
            | Power      cd -> xmitCommand Passive cd
            | Max        cd -> xmitCommand Passive cd
            | Schedule   cd -> xmitCommand roomba.OperatingMode cd
            | SetDayTime cd -> xmitCommand roomba.OperatingMode cd
        | Safe _ ->
            match cleaningModeCommand with
            | Spot       cd -> xmitCommand Passive cd
            | Clean      cd -> xmitCommand Passive cd
            | SeekDock   cd -> xmitCommand Passive cd
            | Power      cd -> xmitCommand Passive cd
            | Max        cd -> xmitCommand Passive cd
            | Schedule   cd -> xmitCommand roomba.OperatingMode cd
            | SetDayTime cd -> xmitCommand roomba.OperatingMode cd
        | Full _ ->
            match cleaningModeCommand with
            | Spot       cd -> xmitCommand Passive cd
            | Clean      cd -> xmitCommand Passive cd
            | SeekDock   cd -> xmitCommand Passive cd
            | Power      cd -> xmitCommand Passive cd
            | Max        cd -> xmitCommand Passive cd
            | Schedule   cd -> xmitCommand roomba.OperatingMode cd
            | SetDayTime cd -> xmitCommand roomba.OperatingMode cd

