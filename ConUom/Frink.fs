namespace ConUom

open System
open System.IO

open FParsec

module private Parser =

#if DEBUG
    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A: %A)" stream.Position label reply.Status reply.Result
            reply
#endif

    let skipComment =
        skipString "//" .>> skipRestOfLine false

    let identifier =
        let isAsciiIdStart c =
            isAsciiLetter c || c = '\\'
        identifier (IdentifierOptions(isAsciiIdStart))

    let skipPrefix =
        identifier
            .>> spaces
            .>> (skipString "::-" <|> skipString ":-")
            .>> skipRestOfLine false
            >>% ()
            |> attempt

    let skipJunk =
        choice [
            spaces1
            skipComment
            skipPrefix
        ] |> many

    let parseBaseUnit =
        pipe5
            identifier
            spaces
            (skipString "=!=")
            spaces
            identifier
            (fun dim _ _ _ name ->
                Unit.createBase dim name)
            |> attempt

    let parse =
        optional skipJunk
            >>. sepBy parseBaseUnit skipJunk
            .>> optional skipJunk

    /// Runs the given parser on the given string.
    let run parser str =
        let parser = parser .>> eof   // force consumption of entire string
        let str = if isNull str then "" else str
        match runParserOnString parser () "" str with
            | Success (result, _, _) -> result
            | Failure (msg, _, _) -> failwith msg

module Frink =

    let parseFile path =
        File.ReadAllText path |> Parser.run Parser.parse
