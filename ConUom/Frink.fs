namespace ConUom

open System
open System.IO

open MathNet.Numerics

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

    let addUnit (name : string) (unit : Unit) =
        updateUserState (Map.add name unit)

    let parseBaseUnit =
        pipe3
            identifier
            (spaces >>. (skipString "=!=") .>> spaces)
            identifier
            (fun dim _ name ->
                name, Unit.createBase dim name)
            |> attempt

    let consumeBaseUnit =
        parse {
            let! name, unit = parseBaseUnit
            do! addUnit name unit
        }

    let parseBigInt =
        many1Satisfy Char.IsDigit
            |>> bigint.Parse

    let parseBigRational =
        parse {
            let! num = parseBigInt
            let! slashOpt = opt (skipChar '/')
            if slashOpt.IsSome then
                let! den = parseBigInt
                return BigRational.FromBigIntFraction(num, den)
            else
                return BigRational.FromBigInt(num)
        }

    let parseDerivedUnit state =
        pipe5
            identifier
            (spaces >>. (skipString ":=") .>> spaces)
            (opt parseBigRational)
            spaces
            identifier
            (fun newName _ scaleOpt _ oldName ->
                let scale = scaleOpt |> Option.defaultValue 1N
                let oldUnit : Unit = state |> Map.find oldName
                let newUnit = scale @@ oldUnit
                newName, newUnit)
            |> attempt

    let consumeDerivedUnit =
        parse {
            let! state = getUserState
            let! name, unit = parseDerivedUnit state
            do! addUnit name unit
        }

    let consumeUnit =
        consumeBaseUnit <|> consumeDerivedUnit

    let parseUnits =
        optional skipJunk
            >>. sepBy consumeUnit skipJunk
            .>> optional skipJunk

    let parse str =
        let parser = parseUnits .>> eof   // force consumption of entire string
        let str = if isNull str then "" else str
        match runParserOnString parser Map.empty "" str with
            | Success (_, result, _) -> result
            | Failure (msg, _, _) -> failwith msg

module Frink =

    let parseFile path =
        File.ReadAllText path |> Parser.parse
