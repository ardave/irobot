namespace iRobot

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

module Actuation =

    type Radius =
    | Straight 
    | TurnInPlaceClockwise
    | TurnInPlaceCounterclockwise
    | ArbitraryRadius of int<mm>

    let getDriveBytes velocity radius =
        let velocityBytes =
            velocity / 1<velocity>
            |> BitStuff.intToTwosComplementBytes
        let radiusBytes =
            match radius with 
            | Straight                    -> 32768 |> BitStuff.intToTwosComplementBytes
            | TurnInPlaceClockwise        ->    -1 |> BitStuff.intToTwosComplementBytes
            | TurnInPlaceCounterclockwise ->     1 |> BitStuff.intToTwosComplementBytes
            | ArbitraryRadius mm          -> 1<mm> |> (/) mm |> BitStuff.intToTwosComplementBytes
        Array.append velocityBytes radiusBytes
        
    let createDriveCommand velocity radius =
        let dataBytes = getDriveBytes velocity radius
        { OpCode = 137uy; DataBytes = dataBytes }

