namespace ConUom

open System
open System.IO

open MathNet.Numerics

open FParsec

/// State maintained by the Frink parser.
type private State =
    {
        /// Prefixes. E.g. "milli" = 1/1000.
        Prefixes : Map<string, BigRational>

        /// Units. E.g. "gram" = 1/1000 kg.
        Units : Map<string, Unit>
    }

module private State =

    /// Finds the unit with the given name.
    let findUnit name state =
        state.Units
            |> Map.tryFind name
            |> Option.defaultWith (fun () ->

                    // try prefixes if name not found
                state.Prefixes
                    |> Map.toSeq
                    |> Seq.tryPick (fun (prefix, scale) ->
                        if name.StartsWith(prefix) then
                            let name' =
                                name.Substring(prefix.Length)
                            state.Units
                                |> Map.tryFind name'
                                |> Option.map (fun unit ->
                                    scale @@ unit)
                        else None)
                    |> Option.defaultWith (fun () ->
                        failwithf "Undefined unit: %s" name))

    /// Finds the prefixe with the given name.
    let findPrefix name state =
        state.Prefixes
            |> Map.tryFind name
            |> Option.defaultWith (fun () ->
                failwithf "Undefined prefix: %s" name)

#if DEBUG
[<AutoOpen>]
module private Debug =

    /// Debug parser.
    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A: %A)" stream.Position label reply.Status reply.Result
            reply
#endif

module private Decimal =

    /// Parses a decimal number.
    let parse =
        parse {
            let! whole = many1Satisfy isDigit
            do! skipChar '.'
            let! fraction = many1Satisfy isDigit
            return sprintf "%s.%s" whole fraction
                |> Decimal.Parse
        } |> attempt

module private BigInt =

    /// Parses a big integer, ignoring underscores.
    let parse =
        many1Satisfy (fun c -> isDigit c || c = '_')
            |>> (fun str -> str.Replace("_", ""))
            |>> bigint.Parse

module private BigRational =

    /// Parses an exact exponential as a rational. E.g. "1ee12".
    let parseExactExponential =
        parse {
            do! skipString "1ee"
            let! exp = pint32
            return 1N ** exp
        }

    /// Parses a decimal as a rational.
    let parseDecimal =
        Decimal.parse
            |>> BigRational.FromDecimal

    /// Parses a fraction (or whole number) as a rational.
    let parseFraction =
        parse {
            let! num = BigInt.parse
            let! slashOpt = opt (skipChar '/')
            let! den =
                if slashOpt.IsSome then BigInt.parse
                else preturn 1I
            return BigRational.FromBigIntFraction(num, den)
        }

    /// Parses a rational.
    let parse =
        choice [
            parseExactExponential
            parseDecimal
            parseFraction   // must be last
        ]

module private FrinkParser =

    /// Skips whitespace characters within a line.
    let spaces =
        skipMany (skipAnyOf " \t")   // don't allow newline

    /// Skips a comment.
    let skipComment =
        choice [

                // a comment like this
            (skipString "//"
                .>> skipRestOfLine false)

                // /* a comment like this */
            (skipString "/*"
                >>. skipCharsTillString "*/" true Int32.MaxValue)
        ]

    /// Parses an identifier.
    let identifier =
        let isAsciiIdStart c =
            isAsciiLetter c || c = '\\'
        choice [
            identifier <| IdentifierOptions(isAsciiIdStart)
            pstring "1"
            pstring "<<IMAGINARY_UNIT>>"
        ]

    /// Skips irrelevant text.
    let skipJunk =
        choice [
            spaces1   // whitespace, including newlines
            skipComment
        ] |> many

    /// Consumes a value produced by the given parser.
    let consume parser consumer =
        parse {
            let! state = getUserState
            let! value = parser state
            return! setUserState <| consumer state value
        }

    /// Consumes a value produced by the given stateless parser.
    let consumeStateless parser consumer =
        consume (fun _ -> parser) consumer

    module Prefix =

        /// Adds the given prefix to the given state.
        let add state (name, scale) =
            { state with
                Prefixes = state.Prefixes |> Map.add name scale }

        /// Parses the declaration of a base prefix. E.g. "milli ::- 1/1000".
        let parseBaseDecl =
            parse {
                let! name = identifier
                do! spaces
                do! skipString "::-"
                do! spaces
                let! scale = BigRational.parse
                return name, scale
            } |> attempt

        /// Parses the declaration of a derived prefix. E.g. "m :- milli".
        let parseDerivedDecl state =
            parse {
                let! newName = identifier
                do! spaces
                do! skipString ":-"
                do! spaces
                let! scaleOpt = opt BigRational.parse
                let! scale =
                    match scaleOpt with
                        | Some value ->
                            preturn value
                        | None ->
                            identifier
                                |>> (fun oldName ->
                                    state |> State.findPrefix oldName)
                return newName, scale
            } |> attempt

        /// Parses a prefix declaration.
        let parseDecl state =
            choice [
                parseBaseDecl
                parseDerivedDecl state
            ]

        /// Consumes a prefix declaration.
        let consumeDecl =
            consume parseDecl add

    module Unit =

        /// Adds the given unit to the given state.
        let add state (name, unit) =
            { state with
                Units = state.Units |> Map.add name unit }

        /// Parses the declaration of a base unit. E.g. "length =!= m".
        let parseBaseDecl =
            parse {
                let! dim = identifier
                do! spaces
                do! skipString "=!="
                do! spaces
                let! name = identifier
                return name, Unit.createBase dim name
            } |> attempt

        /// Parses a unit followed optionally by a power. E.g. "m^2".
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

        /// Parses the product of one or more unit-powers. E.g. "m s^-1".
        let parseProduct state =
            many1 (parseUnitPower state .>> spaces)   // note: consumes trailing spaces
                |>> List.fold (*) Unit.one

        /// Parses a combination of units. E.g. "kg m / s^2".
        let parseCombination state =
            parse {

                    // numerator
                let! num = parseProduct state

                    // optional division
                do! spaces
                let! divOpt =
                    let skipSlash = skipChar '/'
                    let skipDiv =   // distinguish comment from division
                        skipSlash
                            .>> notFollowedBy skipSlash
                            |> attempt
                    opt skipDiv
                do! spaces

                    // denominator
                let! den =
                    if divOpt.IsSome then
                        parseProduct state
                    else preturn Unit.one

                return num / den
            }

        /// Parses the declaration of a derived unit.
        /// E.g. "kilogram := kg"
        /// E.g. "gram := 1/1000 kg"
        /// E.g. "radian := 1"
        let parseDerivedDecl state =
            parse {
                let! newName = identifier
                do! spaces
                do! skipString ":="
                do! spaces
                let! scaleOpt = opt BigRational.parse
                do! spaces
                let! oldUnitOpt = opt (parseCombination state)

                let scale = scaleOpt |> Option.defaultValue 1N
                let oldUnit =
                    oldUnitOpt
                        |> Option.defaultValue Unit.one
                let newUnit = scale @@ oldUnit
                return newName, newUnit
            } |> attempt

        /// Parses the declaration of a combination unit.
        /// E.g. "m^2 K / W ||| thermal_insulance".
        let parseCombinationDecl state =
            parse {
                let! unit = parseCombination state
                do! spaces
                do! skipString "|||"
                do! spaces
                let! name = identifier
                return name, unit
            } |> attempt

        /// Parses a unit declaration.
        let parseDecl state =
            choice [
                parseBaseDecl
                parseDerivedDecl state
                parseCombinationDecl state
            ]

        /// Consumes a unit declaration.
        let consumeDecl =
            consume parseDecl add

    /// Consumes a declaration.
    let consumeDecl =
        choice [
            Prefix.consumeDecl
            Unit.consumeDecl
        ]

    /// Consumes multiple declarations.
    let consumeDecls =
        optional skipJunk
            >>. skipSepBy consumeDecl skipJunk
            .>> optional skipJunk

    /// Parses the given Frink declarations.
    let parse str =
        let parser = consumeDecls .>> eof   // force consumption of entire string
        let str = if isNull str then "" else str
        let state =
            {
                Prefixes = Map.empty
                Units = Map [ "<<IMAGINARY_UNIT>>", Unit.one ]   // ick
            }
        match runParserOnString parser state "" str with
            | Success (_, result, _) -> result.Units 
            | Failure (msg, _, _) -> failwith msg

module Frink =

    /// Parses the given Frink file.
    let parseFile path =
        File.ReadAllText path |> FrinkParser.parse
