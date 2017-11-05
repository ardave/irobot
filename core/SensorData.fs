module Sensors

open BitStuff
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
| Neither

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
    LightBumpLeftSignal          : int option
    LightBumpFrontLeftSignal     : int option
    LightBumpCenterLeftSignal    : int option
    LightBumpCenterRightSignal   : int option
    LightBumpFrontRightSignal    : int option
    LightBumpRightSignal         : int option
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
    LightBumpLeftSignal          = None
    LightBumpFrontLeftSignal     = None
    LightBumpCenterLeftSignal    = None
    LightBumpCenterRightSignal   = None
    LightBumpFrontRightSignal    = None
    LightBumpRightSignal         = None
    LeftMotorCurrent             = None
    RightMotorCurrent            = None
    MainBrushMotorCurrent        = None
    SideBrushMotorCurrent        = None
    Stasis                       = None
}

type PacketGroup = 
| Group100
| LightBumpSensors

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

// todo: Generalize these next few functions, once confident
// that signing/unsigning of its is/are correct:
let parseVoltage (ba:byte array) = 
    let result = ResultBuilder()
    result {
        let! numbers = parseTwoByteWord "Battery Voltage" ba
        let voltage = numbers * 1<mV>
        return voltage
    }
let parseCurrent ba = 
    let result = ResultBuilder()
    result {
        let! numbers = parseTwoByteWord "Battery Current" ba
        let current = numbers * 1<mA>
        return current
    }
let parseTemperature = int
let parseBatteryCharge ba =
    let result = ResultBuilder()
    result {
        let! numbers = parseTwoByteWord "Battery Charge" ba
        let current = numbers * 1<mAh>
        return current
    } 
let parseBatteryCapacity ba =
    let result = ResultBuilder()
    result {
        let! numbers = parseTwoByteWord "Battery Capacity" ba
        let current = numbers * 1<mAh>
        return current
    }
let parseWallSignal            ba = parseTwoByteWord "Wall Signal"              ba
let parseCliffLeftSignal       ba = parseTwoByteWord "Cliff Left Signal"        ba
let parseCliffFrontLeftSignal  ba = parseTwoByteWord "Cliff Front Left Signal"  ba
let parseCliffFrontRightSignal ba = parseTwoByteWord "Cliff Front Right Signal" ba
let parseCliffRightSignal      ba = parseTwoByteWord "Cliff Right Signal"       ba
let parseChargingSourcesAvailable b =
    let bit0 = b |> isBitSet 0
    let bit1 = b |> isBitSet 1
    match (bit0, bit1) with 
    | true , true  -> Both
    | true , false -> InternalCharger
    | false, true  -> HomeBase
    | false, false -> Neither

// Might make more sense to have separate OI Mode for parsed response, vs
// OperatingMode for a command which changes the mode:
let parseOIMode b =
    match int b with
    | 0 -> OperatingMode.Off
    | 1 -> OperatingMode.Passive
    // | 2 -> OperatingMode.createSafe
    | 3 -> OperatingMode.createFull
    | _ -> failwith "Unrecognized OI/Operating Mode"
    
let parseCurrentlySelectedSongNumber = int
let parseSongPlaying b =
    match int b with 
    | 1 -> true
    | _ -> false

let parseNumberOfStreamPackets = int
let parseRequestedVelocity ba =
    let result = ResultBuilder()
    result {
        let! numbers = parseTwoByteWord "Requested Velocity" ba
        let velocity = numbers * 1<velocity>
        return velocity
    }
let parseRequestedRadius ba =
    let result = ResultBuilder()
    result {
        let! numbers = parseTwoByteWord "Requested Radius" ba
        let radius = numbers * 1<mm>
        return radius
    }
// trying once without using Computation Expression, just to see
// how it feels in comparison:
let parseRequestedRightVelocity ba =
    match parseTwoByteWord "Requested Right Velocity" ba with
    | Ok number -> Ok(number * 1<mm>)
    | Error x -> Error(x)



