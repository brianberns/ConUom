namespace ConUom.Test

open ConUom

module Program =

    [<EntryPoint>] 
    let main _ =
        use client = new System.Net.WebClient()
        let lookup, msgOpt =
            client.DownloadString("https://frinklang.org/frinkdata/units.txt")
                |> Frink.parse
        for (key, value) in lookup.Prefixes |> Map.toSeq do
            printfn "%s: %A" key value
        for (key, value) in lookup.Units |> Map.toSeq do
            printfn "%s: %A" key value
        msgOpt |> Option.iter (printfn ""; printfn "%A")
        (*
        try
            TestClass().ParseHubbleConstant()
        with ex -> printfn "%A" ex
        *)
        0
