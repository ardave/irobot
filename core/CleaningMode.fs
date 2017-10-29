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

    type ScheduleTime = {
        Hour  : int
        Minute: int
    }

    type Schedule = {
        Sunday   : ScheduleTime
        Monday   : ScheduleTime
        Tuesday  : ScheduleTime
        Wednesday: ScheduleTime
        Thursday : ScheduleTime
        Friday   : ScheduleTime
        Saturday : ScheduleTime
    }

    let createSpot       = { OpCode = 134uy; DataBytes = Array.empty }
    let createClean      = { OpCode = 135uy; DataBytes = Array.empty }
    let createSeekDock   = { OpCode = 143uy; DataBytes = Array.empty }
    let createPower      = { OpCode = 133uy; DataBytes = Array.empty }
    let createMax        = { OpCode = 136uy; DataBytes = Array.empty }
    let createSchedule schedule  = 
        let setAllWeekdaysAtOnce = 127
        let data = 
            [|
            setAllWeekdaysAtOnce
            schedule.Sunday.Hour
            schedule.Sunday.Minute
            schedule.Monday.Hour
            schedule.Monday.Minute
            schedule.Tuesday.Hour
            schedule.Tuesday.Minute
            schedule.Wednesday.Hour
            schedule.Wednesday.Minute
            schedule.Thursday.Hour
            schedule.Thursday.Minute
            schedule.Friday.Hour
            schedule.Friday.Minute
            schedule.Saturday.Hour
            schedule.Saturday.Minute
            |]
            |> Array.map byte
        { OpCode = 167uy; DataBytes = data }
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

