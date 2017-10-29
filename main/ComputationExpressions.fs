namespace iRobot

type ResultBuilder() =
    member this.Bind(m, f) = 
        match m with
        | Error _ -> m
        | Ok a -> f a

    member this.Return(x) = 
        Ok x