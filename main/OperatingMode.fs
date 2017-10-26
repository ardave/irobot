namespace iRobot

type OperatingMode =
| Off
| Passive
| Safe
| Full


module OMFuncs =
    let getOperatingModeCommandData operatingMode = 
        match operatingMode with 
        | Safe -> { OpCode = 131uy; DataBytes = Array.empty }
        | Full -> { OpCode = 132uy; DataBytes = Array.empty }
        | _    -> failwith "not covered by the manual!"