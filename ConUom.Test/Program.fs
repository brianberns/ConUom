namespace ConUom.Test

open ConUom

module Program =

    [<EntryPoint>] 
    let main _ =
        let pairs, msgOpt = Frink.parseFile "units.txt"
        for (key, value) in pairs |> Map.toSeq do
            printfn "%s: %A" key value
        msgOpt |> Option.iter (printfn ""; printfn "%A")
        0
