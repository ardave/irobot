namespace iRobot

open Communicator
open OperatingMode

// Pg 6 at the top suggests that this type of command might be 
// different from the CleaningModeCommand
type Command =
| Start
| Reset
| Stop
| Baud

module CommandConstructors =
    let getCommandOpCode = function
        | Start -> 128uy
        | Reset ->   7uy
        | Stop  -> 173uy
        | Baud  -> 129uy
    