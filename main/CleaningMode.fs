namespace iRobot

open Communicator
open OperatingMode

type CleaningMode =
| Spot
| Clean
| SeekDock
| Power
| Max
| Schedule
| SetDayTime

module CleaningModeConstructors =
    let getCleaningModeCommandData cleaningMode =
        match cleaningMode with 
        | Spot       -> { OpCode = 134uy; DataBytes = Array.empty  } //✓
        | Clean      -> { OpCode = 135uy; DataBytes = Array.empty  } //✓
        | SeekDock   -> { OpCode = 143uy; DataBytes = Array.empty  } //✓
        | Power      -> { OpCode = 133uy; DataBytes = Array.empty  } //✓
        | Max        -> { OpCode = 136uy; DataBytes = Array.empty  } //✓
        | Schedule   -> { OpCode = 167uy; DataBytes = Array.empty  }
        | SetDayTime -> { OpCode = 168uy; DataBytes = Array.empty  }

    let sendCleaningModeCommand roomba cleaningModeCommand =
        let xmitCommand newMode =
            cleaningModeCommand |> getCleaningModeCommandData |> transmitCommandData
            { roomba with OperatingMode = newMode }
        match roomba.OperatingMode with
        | Off ->
            match cleaningModeCommand with
            | Spot       -> failwith "illegal"
            | Clean      -> failwith "illegal"
            | SeekDock   -> failwith "illegal"
            | Power      -> failwith "illegal"
            | Max        -> failwith "illegal"
            | Schedule   -> failwith "illegal"
            | SetDayTime -> failwith "illegal"
        | Passive ->
            match cleaningModeCommand with
            | Spot       -> xmitCommand Passive
            | Clean      -> xmitCommand Passive
            | SeekDock   -> xmitCommand Passive
            | Power      -> xmitCommand Passive
            | Max        -> xmitCommand Passive
            | Schedule   -> xmitCommand roomba.OperatingMode
            | SetDayTime -> xmitCommand roomba.OperatingMode
        | Safe _ ->
            match cleaningModeCommand with
            | Spot       -> xmitCommand Passive
            | Clean      -> xmitCommand Passive
            | SeekDock   -> xmitCommand Passive
            | Power      -> xmitCommand Passive
            | Max        -> xmitCommand Passive
            | Schedule   -> xmitCommand roomba.OperatingMode
            | SetDayTime -> xmitCommand roomba.OperatingMode
        | Full _ ->
            match cleaningModeCommand with
            | Spot       -> xmitCommand Passive
            | Clean      -> xmitCommand Passive
            | SeekDock   -> xmitCommand Passive
            | Power      -> xmitCommand Passive
            | Max        -> xmitCommand Passive
            | Schedule   -> xmitCommand roomba.OperatingMode
            | SetDayTime -> xmitCommand roomba.OperatingMode

