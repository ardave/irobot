module Sensors

open iRobot

type BumpDrop = {
    WheelDropLeft                : bool
    WheelDropRight               : bool
    BumpLeft                     : bool
    BumpRight                    : bool
}

type Buttons = {
    Clock:    bool
    Schedule: bool
    Day:      bool
    Hour:     bool
    Minute:   bool
    Dock:     bool
    Spot:     bool
    Clean:    bool
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

type SensorData = {
    BumpDrop                     : BumpDrop option
    Wall                         : bool option
    CliffLeft                    : bool option
    CliffFrontLeft               : bool option
    CliffFrontRight              : bool option
    CliffRight                   : bool option
    VirtualWall                  : bool option
    LeftWheelOvercurrent         : bool option
    RightWheelOvercurrent        : bool option
    MainBrushOvercurrent         : bool option
    SideBrushOvercurrent         : bool option
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
    LeftWheelOvercurrent         = None
    RightWheelOvercurrent        = None
    MainBrushOvercurrent         = None
    SideBrushOvercurrent         = None
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

let parse26ByteArray byteArray =
    ()