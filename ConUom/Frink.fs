namespace ConUom

open System
open System.IO

open MathNet.Numerics

open FParsec

module private Parser =

    type State =
        {
            Prefixes : Map<string, BigRational>
            Units : Map<string, Unit>
        }


    module State =

        let findUnit name state =
            state.Units
                |> Map.tryFind name
                |> Option.defaultWith (fun () ->
                    failwithf "Undefined unit: %s" name)

#if DEBUG
    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A: %A)" stream.Position label reply.Status reply.Result
            reply
#endif

    let skipComment =
        choice [
            (skipString "//" .>> skipRestOfLine false)
            (skipString "/*" >>. skipCharsTillString "*/" true Int32.MaxValue)
        ]

    let identifier =
        let isAsciiIdStart c =
            isAsciiLetter c || c = '\\'
        choice [
            identifier <| IdentifierOptions(isAsciiIdStart)
            pstring "1"
            pstring "<<IMAGINARY_UNIT>>"
        ]

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
        updateUserState (fun state ->
            { state with
                Units = state.Units |> Map.add name unit })

    let parseBaseUnit =
        parse {
            let! dim = identifier
            do! spaces
            do! skipString "=!="
            do! spaces
            let! name = identifier
            return name, Unit.createBase dim name
        } |> attempt

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
            let! den =
                if slashOpt.IsSome then parseBigInt
                else preturn 1I
            return BigRational.FromBigIntFraction(num, den)
        }

    let parseExponential =
        parse {
            do! skipString "1ee"
            let! exp = pint32
            return 1N ** exp
        }

    let parseRational =
        parseExponential <|> parseBigRational   // must try exponential first

    /// E.g. kilogram := kg
    /// E.g. gram := 1/1000 kg
    /// E.g. radian := 1
    let parseDerivedUnit state =
        parse {
            let! newName = identifier
            do! spaces
            do! skipString ":="
            do! spaces
            let! scaleOpt = opt parseRational
            do! skipMany (skipAnyOf " \t")   // don't allow newline
            let! oldNameOpt = opt identifier

            let scale = scaleOpt |> Option.defaultValue 1N
            let oldUnit =
                oldNameOpt
                    |> Option.map (fun oldName ->
                        state |> State.findUnit oldName)
                    |> Option.defaultValue Unit.one
            let newUnit = scale @@ oldUnit
            return newName, newUnit
        } |> attempt

    let consumeDerivedUnit =
        parse {
            let! state = getUserState
            let! name, unit = parseDerivedUnit state
            do! addUnit name unit
        }

    let parseUnitPower state =
        parse {
            let! name = identifier
            let! caretOpt = opt (skipChar '^')
            let! power =
                if caretOpt.IsSome then pint32
                else preturn 1

            let oldUnit =
                if name = "1" then Unit.one
                else state.Units.[name]
            return oldUnit ^ power
        } |> attempt

    let parseCombination state =
        many (parseUnitPower state .>> spaces)
            |>> (List.fold (*) Unit.one)

    let parseCombinationUnit state =
        parse {
            let! unit = parseCombination state
            do! spaces
            do! skipString "|||"
            do! spaces
            let! name = identifier
            return name, unit
        } |> attempt

    let consumeCombinationUnit =
        parse {
            let! state = getUserState
            let! name, unit = parseCombinationUnit state
            do! addUnit name unit
        }

    let consumeUnit =
        choice [
            consumeBaseUnit
            consumeDerivedUnit
            consumeCombinationUnit
        ]

    let parseUnits =
        optional skipJunk
            >>. sepBy consumeUnit skipJunk
            .>> optional skipJunk

    let parse str =
        let parser = parseUnits .>> eof   // force consumption of entire string
        let str = if isNull str then "" else str
        let state =
            {
                Prefixes = Map.empty
                Units = Map [ "<<IMAGINARY_UNIT>>", Unit.one ]
            }
        match runParserOnString parser state "" str with
            | Success (_, result, _) -> result.Units 
            | Failure (msg, _, _) -> failwith msg

module Frink =

    let parseFile path =
        File.ReadAllText path |> Parser.parse
