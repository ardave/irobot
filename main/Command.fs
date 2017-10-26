namespace iRobot

// Pg 6 at the top suggests that this type of command might be 
// different from the CleaningModeCommand
type Command =
| Start of CommandData
| Reset of CommandData
| Stop of CommandData
| Baud of CommandData

module CommandConstructors =
    let createStart = Start <| { OpCode = 128uy; DataBytes = Array.empty }
    let createReset = Reset <| { OpCode =   7uy; DataBytes = Array.empty }
    let createStop  = Stop  <| { OpCode = 173uy; DataBytes = Array.empty }
    let createBaud (baudRate:BaudRate) = Baud <| { OpCode = 129uy; DataBytes = [|byte baudRate|] }
