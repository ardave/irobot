open RJCP.IO.Ports

let tryIt() =
    let src = new SerialPortStream("/dev/ttyS0")
    src.Dispose()

[<EntryPoint>]
let main argv = 
    tryIt()
    0