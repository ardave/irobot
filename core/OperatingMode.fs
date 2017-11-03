[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OperatingMode

open iRobot

// I think I need to distinguish between OperatingMode *state*,
// and operating mode *commands.*
type OperatingMode =
| Off
| Passive
| Safe of CommandData
| Full of CommandData

let createSafe = { OpCode = 131uy; DataBytes = Array.empty }
let createFull = Full <| { OpCode = 132uy; DataBytes = Array.empty }