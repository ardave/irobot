namespace iRobot

type InputCommand =
| Sensors
| QueryList
| Stream
| PauseResumeStream

module ConstructorFunctions =
    let getInputCommandData inputCommand =
        match inputCommand with 
        | Sensors           -> { OpCode = 142uy; DataBytes = Array.empty }
        | QueryList         -> { OpCode = 149uy; DataBytes = Array.empty } // actually N + 1, where N is the number of packets requested. 
        | Stream            -> { OpCode = 148uy; DataBytes = Array.empty } // actually N + 1, where N is the number of packets requested.
        | PauseResumeStream -> { OpCode = 150uy; DataBytes = Array.empty }
