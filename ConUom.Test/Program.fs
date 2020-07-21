namespace ConUom.Test

open ConUom

module Program =

    [<EntryPoint>] 
    let main _ =
        let unitMap, msgOpt = Frink.parseFile "units.txt"
        for (key, value) in unitMap |> Map.toSeq do
            printfn "%s: %A" key value
        msgOpt |> Option.iter (printfn ""; printfn "%A")
        0
