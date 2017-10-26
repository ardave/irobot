namespace iRobot

type Roomba = {
    OperatingMode: OperatingMode
}

module DefinitelyMisplaced =
    let sendModeCommand roomba command = 
        match command with 
        | _ -> { roomba with OperatingMode = Passive }