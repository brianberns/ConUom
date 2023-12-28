namespace ConUom.Test

open ConUom

module Program =

    [<EntryPoint>] 
    let main _ =
        use client = new System.Net.Http.HttpClient()
        let lookup, msgOpt =
            task {
                let! str = client.GetStringAsync("https://frinklang.org/frinkdata/units.txt")
                return Frink.parse str
            } |> Async.AwaitTask |> Async.RunSynchronously
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
