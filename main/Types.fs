module Types

open iRobot

type OperatingMode =
| Off
| Passive
| Safe
| Full

type CleaningMode =
| Spot
| Clean
| SeekDock
| Power
| Max
| Schedule
| SetDayTime

type ActuatorCommand =
| Drive
| DriveDirect
| DrivePwm
| Motors
| PwmMotors
| LEDs
| SchedulingLEDs
| DigitalLEDsRaw
| Buttons
| DigitLEDsASCII
| Song
| Play 

type InputCommand =
| Sensors
| QueryList
| Stream
| PauseResumeStream

type Roomba = {
    OperatingMode: OperatingMode
}

let getOperatingModeCommandData operatingMode = 
    match operatingMode with 
    | Safe -> { OpCode = 131uy; DataBytes = Array.empty }
    | Full -> { OpCode = 132uy; DataBytes = Array.empty }
    | _    -> failwith "not covered by the manual!"

let getCleaningModeCommandData cleaningMode =
    match cleaningMode with 
    | Spot       -> { OpCode = 134uy; DataBytes = Array.empty  } //✓
    | Clean      -> { OpCode = 135uy; DataBytes = Array.empty  } //✓
    | SeekDock   -> { OpCode = 143uy; DataBytes = Array.empty  } //✓
    | Power      -> { OpCode = 133uy; DataBytes = Array.empty  } //✓
    | Max        -> { OpCode = 136uy; DataBytes = Array.empty  } //✓
    | Schedule   -> { OpCode = 167uy; DataBytes = Array.empty  }
    | SetDayTime -> { OpCode = 168uy; DataBytes = Array.empty  }

let getInputCommandData inputCommand =
    match inputCommand with 
    | Sensors           -> { OpCode = 142uy; DataBytes = Array.empty }
    | QueryList         -> { OpCode = 149uy; DataBytes = Array.empty } // actually N + 1, where N is the number of packets requested. 
    | Stream            -> { OpCode = 148uy; DataBytes = Array.empty } // actually N + 1, where N is the number of packets requested.
    | PauseResumeStream -> { OpCode = 150uy; DataBytes = Array.empty }

let transmitCommandData commandData =
    let dataSequence = Array.append [|commandData.OpCode|] commandData.DataBytes
    printfn "Transmitting CommandData: %A" dataSequence

let sendCommand roomba command =
    let xmitCommand commandData newMode = 
        commandData |> transmitCommandData
        { roomba with OperatingMode = newMode}
    match roomba.OperatingMode with 
    | Off ->
        match command with
        | Start commandData -> xmitCommand commandData Passive
        | Reset commandData -> xmitCommand commandData Off
        | Stop  _ -> failwith "illegal (but does it matter?)"
        | Baud  _ -> failwith "illegal (but does it matter?)"
    | Passive ->
        match command with
        | Start commandData -> xmitCommand commandData Passive
        | Reset commandData -> xmitCommand commandData Off
        | Stop  commandData -> xmitCommand commandData Off
        | Baud  commandData -> xmitCommand commandData roomba.OperatingMode
    | Safe ->
        match command with
        | Start commandData -> xmitCommand commandData Passive
        | Reset commandData -> xmitCommand commandData Off
        | Stop  commandData -> xmitCommand commandData Off
        | Baud  commandData -> xmitCommand commandData roomba.OperatingMode
    | Full ->
        match command with
        | Start commandData -> xmitCommand commandData Passive
        | Reset commandData -> xmitCommand commandData Off
        | Stop  commandData -> xmitCommand commandData Off
        | Baud  commandData -> xmitCommand commandData roomba.OperatingMode

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
    | Safe ->
        match cleaningModeCommand with
        | Spot       -> xmitCommand Passive
        | Clean      -> xmitCommand Passive
        | SeekDock   -> xmitCommand Passive
        | Power      -> xmitCommand Passive
        | Max        -> xmitCommand Passive
        | Schedule   -> xmitCommand roomba.OperatingMode
        | SetDayTime -> xmitCommand roomba.OperatingMode
    | Full ->
        match cleaningModeCommand with
        | Spot       -> xmitCommand Passive
        | Clean      -> xmitCommand Passive
        | SeekDock   -> xmitCommand Passive
        | Power      -> xmitCommand Passive
        | Max        -> xmitCommand Passive
        | Schedule   -> xmitCommand roomba.OperatingMode
        | SetDayTime -> xmitCommand roomba.OperatingMode


let sendModeCommand roomba command = 
    match command with 
    | _ -> { roomba with OperatingMode = Passive }

