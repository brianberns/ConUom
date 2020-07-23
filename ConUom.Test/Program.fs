namespace ConUom.Test

open ConUom

module Program =

    [<EntryPoint>] 
    let main _ =
        let lookup, msgOpt = Frink.parseFile "units.txt"
        for (key, value) in lookup.Prefixes |> Map.toSeq do
            printfn "%s: %A" key value
        for (key, value) in lookup.Units |> Map.toSeq do
            printfn "%s: %A" key value
        msgOpt |> Option.iter (printfn ""; printfn "%A")
        0
