namespace ConUom.Test

open ConUom

module Program =

    [<EntryPoint>] 
    let main _ =
        match Frink.parseFile "units.txt" with
            | Ok pairs ->
                for (key, value) in pairs |> Map.toSeq do
                    printfn "%s: %A" key value
            | Error msg ->
                printfn "%A" msg
        0
