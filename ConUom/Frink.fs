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

        let findPrefix name state =
            state.Prefixes
                |> Map.tryFind name
                |> Option.defaultWith (fun () ->
                    failwithf "Undefined prefix: %s" name)

#if DEBUG
    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A: %A)" stream.Position label reply.Status reply.Result
            reply
#endif

    let spaces =
        skipMany (skipAnyOf " \t")   // don't allow newline

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

    let skipJunk =
        choice [
            spaces1
            skipComment
        ] |> many

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

    let addPrefix state (name, value) =
        { state with
            Prefixes = state.Prefixes |> Map.add name value }

    let parseBasePrefix =
        parse {
            let! name = identifier
            do! spaces
            do! skipString "::-"
            do! spaces
            let! value = parseRational
            return name, value
        } |> attempt

    let parseDerivedPrefix state =
        parse {
            let! newName = identifier
            do! spaces
            do! skipString ":-"
            do! spaces
            let! valueOpt = opt parseRational
            let! value =
                match valueOpt with
                    | Some value ->
                        preturn value
                    | None ->
                        identifier
                            |>> (fun oldName ->
                                state |> State.findPrefix oldName)
            return newName, value
        } |> attempt

    let parsePrefix state =
        parseBasePrefix <|> (parseDerivedPrefix state)

    let consume parser add =
        parse {
            let! state = getUserState
            let! value = parser state
            return! setUserState <| add state value
        }

    let consumeStateless parser add =
        consume (fun _ -> parser) add

    let consumePrefix =
        consume parsePrefix addPrefix

    let addUnit state (name, unit) =
        { state with
            Units = state.Units |> Map.add name unit }

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
        consumeStateless parseBaseUnit addUnit

    let parseUnitPower state =
        parse {
            let! name = identifier
            let! caretOpt = opt (skipChar '^')
            let! power =
                if caretOpt.IsSome then pint32
                else preturn 1

            let unit =
                if name = "1" then Unit.one
                else state |> State.findUnit name
            return unit ^ power
        } |> attempt

    let parseProduct state =
        many1 (parseUnitPower state .>> spaces)
            |>> (List.fold (*) Unit.one)

    let parseCombination state =
        parse {
            let! num = parseProduct state
            do! spaces
            let! divOpt =
                let skipSlash = skipChar '/'
                let skipDiv =   // distinguish comment from division
                    skipSlash
                        .>> notFollowedBy skipSlash
                        |> attempt
                opt skipDiv
            do! spaces
            let! den =
                if divOpt.IsSome then
                    parseProduct state
                else preturn Unit.one
            return num / den
        }

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
            do! spaces
            let! oldUnitOpt = opt (parseCombination state)

            let scale = scaleOpt |> Option.defaultValue 1N
            let oldUnit =
                oldUnitOpt
                    |> Option.defaultValue Unit.one
            let newUnit = scale @@ oldUnit
            return newName, newUnit
        } |> attempt

    let consumeDerivedUnit =
        consume parseDerivedUnit addUnit

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
        consume parseCombinationUnit addUnit

    let consumeUnit =
        choice [
            consumePrefix
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
