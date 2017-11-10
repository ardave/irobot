module SensorPacket

type SensorPacket = 
| BumpDrop                     
| Wall                         
| CliffLeft                    
| CliffFrontLeft               
| CliffFrontRight              
| CliffRight                   
| VirtualWall                  
| WheelOvercurrents            
| DirtDetect                   
| InfraredCharacterOmni        
| InfraredCharacterLeft        
| InfraredCharacterRight       
| Buttons                      
| Distance                     
| Angle                        
| ChargingState                
| BatteryVoltage               
| BatteryCurrent               
| BatteryTemperature           
| BatteryCharge                
| BatteryCapacity              
| WallSignal                   
| CliffLeftSignal              
| CliffFrontLeftSignal         
| CliffFrontRightSignal        
| CliffRightSignal             
| ChargingSourcesAvailable     
| OIMode                       
| SelectedSongNumber           
| PlayingSongNumber            
| NumberOfStringPacketsReturned
| RequestedVelocity            
| RequestedRadius              
| RequestedRightVelocity       
| RequestedLeftVelocity        
| LeftEncoderCounts            
| RightEncoderCounts           
| LightBumper                  
| LightBumpLeftSignal          
| LightBumpFrontLeftSignal     
| LightBumpCenterLeftSignal    
| LightBumpCenterRightSignal   
| LightBumpFrontRightSignal    
| LightBumpRightSignal         
| LeftMotorCurrent             
| RightMotorCurrent            
| MainBrushMotorCurrent        
| SideBrushMotorCurrent        
| Stasis                       

let getId = function
| BumpDrop                      -> 7uy
| Wall                          -> 8uy
| CliffLeft                     -> 9uy
| CliffFrontLeft                -> 10uy
| CliffFrontRight               -> 11uy
| CliffRight                    -> 12uy
| VirtualWall                   -> 13uy
| WheelOvercurrents             -> 14uy
| DirtDetect                    -> 15uy
| InfraredCharacterOmni         -> 17uy
| InfraredCharacterLeft         -> 52uy
| InfraredCharacterRight        -> 53uy
| Buttons                       -> 18uy
| Distance                      -> 19uy
| Angle                         -> 20uy
| ChargingState                 -> 21uy
| BatteryVoltage                -> 22uy
| BatteryCurrent                -> 23uy
| BatteryTemperature            -> 24uy
| BatteryCharge                 -> 25uy
| BatteryCapacity               -> 26uy
| WallSignal                    -> 27uy
| CliffLeftSignal               -> 28uy
| CliffFrontLeftSignal          -> 29uy
| CliffFrontRightSignal         -> 30uy
| CliffRightSignal              -> 31uy
| ChargingSourcesAvailable      -> 34uy
| OIMode                        -> 35uy
| SelectedSongNumber            -> 36uy
| PlayingSongNumber             -> 37uy
| NumberOfStringPacketsReturned -> 38uy
| RequestedVelocity             -> 39uy
| RequestedRadius               -> 40uy
| RequestedRightVelocity        -> 41uy
| RequestedLeftVelocity         -> 42uy
| LeftEncoderCounts             -> 43uy
| RightEncoderCounts            -> 44uy
| LightBumper                   -> 45uy
| LightBumpLeftSignal           -> 46uy
| LightBumpFrontLeftSignal      -> 47uy
| LightBumpCenterLeftSignal     -> 48uy
| LightBumpCenterRightSignal    -> 49uy
| LightBumpFrontRightSignal     -> 50uy
| LightBumpRightSignal          -> 51uy
| LeftMotorCurrent              -> 54uy
| RightMotorCurrent             -> 55uy
| MainBrushMotorCurrent         -> 56uy
| SideBrushMotorCurrent         -> 57uy
| Stasis                        -> 58uy