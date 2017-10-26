namespace iRobot

open OperatingMode

type Roomba = {
    OperatingMode: OperatingMode
}

module Roomba =
    let sendModeCommand roomba command = 
        match command with 
        | _ -> { roomba with OperatingMode = Passive }