module Types

type OperatingMode =
| Off
| Passive
| Safe
| Full

type CommandData = {
    OpCode: int
    DataBytes: int
}

// Pg 6 at the top suggests that this type of command might be 
// different from the CleaningModeCommand
type Command =
| Start
| Reset
| Stop
| Baud

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

let getCommandData command =
    match command with
    | Start -> { OpCode = 128; DataBytes = 0 }
    | Reset -> { OpCode =   7; DataBytes = 0 }
    | Stop  -> { OpCode = 173; DataBytes = 0 }
    | Baud  -> { OpCode = 129; DataBytes = 1 }

let getOperatingModeCommandData operatingMode = 
    match operatingMode with 
    | Safe -> { OpCode = 131; DataBytes = 0 }
    | Full -> { OpCode = 132; DataBytes = 0 }
    | _    -> failwith "not covered by manual!"

let getCleaningModeCommandData cleaningMode =
    match cleaningMode with 
    | Spot       -> { OpCode = 134; DataBytes = 0  }
    | Clean      -> { OpCode = 135; DataBytes = 0  }
    | SeekDock   -> { OpCode = 143; DataBytes = 0  }
    | Power      -> { OpCode = 133; DataBytes = 0  }
    | Max        -> { OpCode = 136; DataBytes = 0  }
    | Schedule   -> { OpCode = 167; DataBytes = 15 }
    | SetDayTime -> { OpCode = 168; DataBytes = 3  }

let getInputCommandData inputCommand =
    match inputCommand with 
    | Sensors           -> { OpCode = 142; DataBytes = 1 }
    | QueryList         -> { OpCode = 149; DataBytes = 1 } // actually N + 1, where N is the number of packets requested. 
    | Stream            -> { OpCode = 148; DataBytes = 1 } // actually N + 1, where N is the number of packets requested.
    | PauseResumeStream -> { OpCode = 150; DataBytes = 1 }

let transmitCommandData (commandData:CommandData) =
    printfn "Transmitting CommandData: %A" commandData

let sendCommand roomba command =
    let xmitCommand newMode = 
        command |> getCommandData |> transmitCommandData
        { roomba with OperatingMode = newMode}
    match roomba.OperatingMode with 
    | Off ->
        match command with
        | Start -> xmitCommand Passive
        | Reset -> xmitCommand Off
        | Stop  -> failwith "illegal (but does it matter?)"
        | Baud  -> failwith "illegal (but does it matter?)"
    | Passive ->
        match command with
        | Start -> xmitCommand Passive
        | Reset -> xmitCommand Off
        | Stop  -> xmitCommand Off
        | Baud  -> xmitCommand roomba.OperatingMode
    | Safe ->
        match command with
        | Start -> xmitCommand Passive
        | Reset -> xmitCommand Off
        | Stop  -> xmitCommand Off
        | Baud  -> xmitCommand roomba.OperatingMode
    | Full ->
        match command with
        | Start -> xmitCommand Passive
        | Reset -> xmitCommand Off
        | Stop  -> xmitCommand Off
        | Baud  -> xmitCommand roomba.OperatingMode

let sendModeCommand roomba command = 
    match command with 
    | _ -> { roomba with OperatingMode = Passive }

