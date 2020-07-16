namespace ConUom.Test

open ConUom

module Program =

    [<EntryPoint>] 
    let main _ =
        printfn "%A" <| Frink.parseFile "units.txt"
        0
