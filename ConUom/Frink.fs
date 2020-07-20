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

    /// Tries to find the unit with the given name.
    let tryFindUnit name state =
        state.Units
            |> Map.tryFind name
            |> Option.orElseWith (fun () ->

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
                        else None))

    /// Tries to find the prefix with the given name.
    let tryFindPrefix name state =
        state.Prefixes
            |> Map.tryFind name

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

    /// Parses a positive decimal number. E.g. "1.23", "1.23e-4".
    let parse =
        parse {
            let! whole = many1Satisfy isDigit
            do! skipChar '.'
            let! fraction = many1Satisfy isDigit
            let n =
                sprintf "%s.%s" whole fraction
                    |> Decimal.Parse

            let! eOpt = opt (skipChar 'e')
            if eOpt.IsSome then
                let! exp = pint32
                return decimal <| (float n) * (10.0 ** (float exp))   // find a better way?
            else
                return n
        } |> attempt

module private BigInt =

    /// Parses a positive big integer, ignoring underscores.
    let parse =
        many1Satisfy (fun c -> isDigit c || c = '_')
            |>> (fun str -> str.Replace("_", ""))
            |>> bigint.Parse

module private BigRational =

    /// Parses a positive exact exponential as a rational. E.g. "1ee12".
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

        let parseDen =
            parse {
                do! skipChar '/'
                return! BigInt.parse
            } |> attempt

        parse {
            let! num = BigInt.parse
            let! den = parseDen <|>% 1I
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

    /// Fails with a formatted string.
    let failf fmt = Printf.ksprintf fail fmt

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
            let! value = parser
            let! state = getUserState
            return! setUserState <| consumer state value
        }

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

        /// Parses a prefix reference.
        let parseRef =
            parse {
                let! name = identifier
                let! state = getUserState
                let scaleOpt = state |> State.tryFindPrefix name
                return!
                    match scaleOpt with
                        | Some scale -> preturn scale
                        | None -> failf "Unknown prefix: %s" name
            } |> attempt

        /// Parses the declaration of a derived prefix. E.g. "m :- milli".
        let parseDerivedDecl =
            parse {
                let! newName = identifier
                do! spaces
                do! skipString ":-"
                do! spaces
                let! scaleOpt = opt BigRational.parse
                let! scale =
                    match scaleOpt with
                        | Some value -> preturn value
                        | None -> parseRef
                return newName, scale
            } |> attempt

        /// Parses a prefix declaration.
        let parseDecl =
            choice [
                parseBaseDecl
                parseDerivedDecl
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

        /// Parses a unit reference.
        let parseRef =
            parse {
                let! name = identifier
                let! state = getUserState
                let unitOpt = state |> State.tryFindUnit name
                return!
                    match unitOpt with
                        | Some unit -> preturn unit
                        | None -> failf "Unknown unit: %s" name
            } |> attempt

        /// Parses a dimensionless unit.
        let parseDimensionless =
            BigRational.parse
                |>> Unit.createScale

        let parseTerm =
            choice [
                parseDimensionless
                parseRef
            ]

        /// Parses an explicit power.
        let parsePower =
            parse {
                do! skipChar '^'
                do! spaces
                return! pint32 // BigRational.parse
            } |> attempt

        /// Parses a term followed optionally by a power. E.g. "m^2".
        let parseTermPower =
            parse {
                let! unit = parseTerm
                do! spaces
                let! power = parsePower <|>% 1
                return unit ^ power
            } |> attempt

        /// Parses the product of one or more term-powers. E.g. "m s^-1".
        let parseProduct =
            many1 (parseTermPower .>> spaces)   // note: consumes trailing spaces
                |>> List.fold (*) Unit.one

        /// Parses a unified product followed optionally by a power. E.g. "(hbar c / G)^(1/2)"
        let parseUnifiedProduct =
            parse {
                do! skipChar '('
                do! spaces
                let! unit = parseProduct
                do! spaces
                do! skipChar ')'
                do! spaces
                let! power = parsePower
                return unit ^ power
            }

        let parseUnified =
            choice [
                parseUnifiedProduct
                parseTermPower
            ]

        let parseQuotient =
            parse {
                let! num = parseProduct <|> parseUnified
                do! spaces
                do! skipChar '/'
                do! spaces
                let! den = parseUnified
                return num/den
            } |> attempt

        let parseExpr =
            choice [
                parseQuotient
                parseProduct
                parseUnified
            ]

        let parseExprProduct =
            many1 parseExpr
                |>> List.fold Unit.mult Unit.one

        /// Parses the declaration of a derived unit.
        /// E.g. "kilogram := kg"
        /// E.g. "gram := 1/1000 kg"
        /// E.g. "radian := 1"
        let parseDerivedDecl =
            parse {
                let! name = identifier
                do! spaces
                do! skipString ":="
                do! spaces
                let! unit = parseExprProduct
                return name, unit
            } |> attempt

        /// Parses the declaration of a combination unit.
        /// E.g. "m^2 K / W ||| thermal_insulance".
        let parseCombinationDecl =
            parse {
                let! unit = parseExpr
                do! spaces
                do! skipString "|||"
                do! spaces
                let! name = identifier
                return name, unit
            } |> attempt

        /// Parses a unit declaration.
        let parseDecl =
            choice [
                parseBaseDecl
                parseDerivedDecl
                parseCombinationDecl
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
        skipJunk
            >>. skipMany (consumeDecl .>> skipJunk)

    /// Parses the given Frink declarations.
    let parse str =
        let parser = consumeDecls .>> eof   // force consumption of entire string
        let str = if isNull str then "" else str
        let state =
            {
                Prefixes = Map.empty
                Units =
                    Map [
                        "<<IMAGINARY_UNIT>>", Unit.one   // ick
                    ]
            }
        match runParserOnString parser state "" str with
            | Success ((), state', _) -> Result.Ok state'.Units
            | Failure (msg, _, _) -> Result.Error msg

module Frink =

    /// Parses the given Frink file.
    let parseFile path =
        File.ReadAllText path |> FrinkParser.parse
