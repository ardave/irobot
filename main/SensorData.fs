module Sensors

open iRobot

type BumpDrop = {
    WheelDropLeft  : bool
    WheelDropRight : bool
    BumpLeft       : bool
    BumpRight      : bool
}

type WheelOvercurrents = {
    LeftWheelOvercurrent  : bool
    RightWheelOvercurrent : bool
    MainBrushOvercurrent  : bool
    SideBrushOvercurrent  : bool
}

type Buttons = {
    Clock   : bool
    Schedule: bool
    Day     : bool
    Hour    : bool
    Minute  : bool
    Dock    : bool
    Spot    : bool
    Clean   : bool
}

type ChargingState =
| NotCharging
| ReconditioningCharging
| FullCharging
| TrickleCharging
| Waiting
| ChargingFaultCondition 

type ChargingSource =
| InternalCharger
| HomeBase
| Both

type LightBumper = {
    Right      : bool
    FrontRight : bool
    CenterRight: bool
    CenterLeft : bool
    FrontLeft  : bool
    Left       : bool
}

// Possible categories for partitioning this huge record type:
//
// Physical sense: Sensors that detect the environment, like bump, cliff, and wall sensors
// Buttons and internal sense: The state of the panel and remote buttons, and the computed
// distance and angle values
// Power sense: The state of the battery and charging systems
 
type SensorData = {
    BumpDrop                     : BumpDrop option
    Wall                         : bool option
    CliffLeft                    : bool option
    CliffFrontLeft               : bool option
    CliffFrontRight              : bool option
    CliffRight                   : bool option
    VirtualWall                  : bool option
    WheelOvercurrents            : WheelOvercurrents option
    DirtDetect                   : int option
    InfraredCharacterOmni        : int option
    InfraredCharacterLeft        : int option
    InfraredCharacterRight       : int option
    Buttons                      : Buttons option
    Distance                     : int<mm> option
    Angle                        : int<degrees> option
    ChargingState                : ChargingState option
    BatteryVoltage               : int<mV> option
    BatteryCurrent               : int<mA> option
    BatteryTemperature           : int<degC> option
    BatteryCharge                : int<mAh> option
    BatteryCapacity              : int<mAh> option
    WallSignal                   : int option
    CliffLeftSignal              : int option
    CliffFrontLeftSignal         : int option
    CliffFrontRightSignal        : int option
    CliffRightSignal             : int option
    ChargingSourcesAvailable     : ChargingSource option
    OIMode                       : OperatingMode.OperatingMode option
    SelectedSongNumber           : int option
    PlayingSongNumber            : int option
    NumberOfStringPacketsReturned: int option
    RequestedVelocity            : int<velocity> option
    RequestedRadius              : int<mm> option
    RequestedRightVelocity       : int<velocity> option
    RequestedLeftVelocity        : int<velocity> option
    LeftEncoderCounts            : int option
    RightEncoderCounts           : int option
    LightBumper                  : LightBumper option
    LeftMotorCurrent             : int<mA> option
    RightMotorCurrent            : int<mA> option
    MainBrushMotorCurrent        : int<mA> option
    SideBrushMotorCurrent        : int<mA> option
    Stasis                       : bool option
}

let defaultSensorData = {
    BumpDrop                     = None
    Wall                         = None
    CliffLeft                    = None
    CliffFrontLeft               = None
    CliffFrontRight              = None
    CliffRight                   = None
    VirtualWall                  = None
    WheelOvercurrents            = None
    DirtDetect                   = None
    InfraredCharacterOmni        = None
    InfraredCharacterLeft        = None
    InfraredCharacterRight       = None
    Buttons                      = None
    Distance                     = None
    Angle                        = None
    ChargingState                = None
    BatteryVoltage               = None
    BatteryCurrent               = None
    BatteryTemperature           = None
    BatteryCharge                = None
    BatteryCapacity              = None
    WallSignal                   = None
    CliffLeftSignal              = None
    CliffFrontLeftSignal         = None
    CliffFrontRightSignal        = None
    CliffRightSignal             = None
    ChargingSourcesAvailable     = None
    OIMode                       = None
    SelectedSongNumber           = None
    PlayingSongNumber            = None
    NumberOfStringPacketsReturned= None
    RequestedVelocity            = None
    RequestedRadius              = None
    RequestedRightVelocity       = None
    RequestedLeftVelocity        = None
    LeftEncoderCounts            = None
    RightEncoderCounts           = None
    LightBumper                  = None
    LeftMotorCurrent             = None
    RightMotorCurrent            = None
    MainBrushMotorCurrent        = None
    SideBrushMotorCurrent        = None
    Stasis                       = None
}

let inline isBitSet pos b =
    b &&& (byte 1 <<< pos) <> byte 0

let parseTwoByteWord opName byteArray =
    // I do not think I have yet accounted for the possibility of the result
    // being signed vs unsigned
    match byteArray |> Array.length with
    | 2 -> 
        byteArray
        |> Array.map int
        |> fun intArray -> (intArray.[1] <<< 8) ||| intArray.[0]
        |> Ok
    | x -> 
        Error (sprintf "Expected %s to be 2-byte array, but length was %i" opName x)

let parseBumpsWheeldrops b =
    {
        BumpRight      = b |> isBitSet 0
        BumpLeft       = b |> isBitSet 1
        WheelDropRight = b |> isBitSet 2
        WheelDropLeft  = b |> isBitSet 3
    }

let private firstBitOfByteToBool b = b |> isBitSet 0
let parseWall = firstBitOfByteToBool
let parseCliffLeft = firstBitOfByteToBool
let parseCliffFrontLeft = firstBitOfByteToBool
let parseCliffFrontRight = firstBitOfByteToBool
let parseCliffRight = firstBitOfByteToBool
let parseVirtualWall = firstBitOfByteToBool
let parseWheelOvercurrents b =
    {
        SideBrushOvercurrent  = b |> isBitSet 0
        MainBrushOvercurrent  = b |> isBitSet 1
        RightWheelOvercurrent = b |> isBitSet 3
        LeftWheelOvercurrent  = b |> isBitSet 4
    }
let parseDirtDetect (b:byte) = int b
let parseInfraredCharacterOmni (b:byte) = int b
let parseInfraredCharacterLeft (b:byte) = int b
let parseInfraredCharacterRight (b:byte) = int b
let parseButtons b =
    {
        Clock    = b |> isBitSet 7
        Schedule = b |> isBitSet 6
        Day      = b |> isBitSet 5
        Hour     = b |> isBitSet 4
        Minute   = b |> isBitSet 3
        Dock     = b |> isBitSet 2
        Spot     = b |> isBitSet 1
        Clean    = b |> isBitSet 0
    }
let parseDistance = parseTwoByteWord "Distance"
let parseAngle = parseTwoByteWord "Angle"
let parseChargingState (b:byte) =
    match int b with 
    | 0 -> NotCharging
    | 1 -> ReconditioningCharging
    | 2 -> FullCharging
    | 3 -> TrickleCharging
    | 4 -> Waiting
    | 5 -> ChargingFaultCondition
    | x -> failwithf "Unknown charging state of %i" x

let parseVoltage (ba:byte array) = 
    let result = ResultBuilder()
    result {
        let! voltage = parseTwoByteWord "Battery Voltage" ba
        return voltage
    }

    // parseTwoByteWord "Battery Voltage" ba
    // |> Result.map <| (*) 1.<mV>
let parseCurrent = parseTwoByteWord "Battery Current"
 







