F# interface to [iRobot Create 2 Robotics kit](http://store.irobot.com/default/create-programmable-programmable-robot-irobot-create-2/RC65099.html)

Initially targeting Mono over Dotnet Core due to better Raspberry Pi/ARM compatibility.

Serial IO uses https://github.com/jcurl/SerialPortStream. 
  It is recommended to build one's own copy of libnserial.so.1 using the instructions under 
  the _"Or you can build and install in your system:"_ section of the main SerialPortStream
  readme.  Also, I have found that this   final step to be necessary: 
    https://github.com/dotnet/core/issues/740#issuecomment-314558139