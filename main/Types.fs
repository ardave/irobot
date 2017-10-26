module Types

type OperatingMode =
| Off
| Passive
| Safe
| Full

type CommandData = {
    OpCode: byte
    DataBytes: byte array
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

type Baud =
| ``300``    =  0uy
| ``600``    =  1uy
| ``1200``   =  2uy
| ``2400``   =  3uy
| ``4800``   =  4uy
| ``9600``   =  5uy
| ``14400``  =  6uy
| ``19200``  =  7uy
| ``28800``  =  8uy
| ``38400``  =  9uy
| ``57600``  = 10uy
| ``115200`` = 11uy

type Roomba = {
    OperatingMode: OperatingMode
}

let getCommandData command =
    match command with
    | Start -> { OpCode = 128uy; DataBytes = Array.empty } //✓
    | Reset -> { OpCode =   7uy; DataBytes = Array.empty } //✓
    | Stop  -> { OpCode = 173uy; DataBytes = Array.empty } //✓
    | Baud  -> { OpCode = 129uy; DataBytes = [|byte Baud.``115200``|] }

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

