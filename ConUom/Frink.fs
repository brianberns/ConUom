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

        let tryFind name =
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

        tryFind name

                // try converting plural to singular
            |> Option.orElseWith (fun () ->
                if name.EndsWith('s') then
                    tryFind (name.Substring(0, name.Length-1))
                else None)

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

    /// Parses a simple positive decimal number. E.g. "1.23".
    let parse =
        parse {
            let! whole = many1Satisfy isDigit
            do! skipChar '.'
            let! fraction = many1Satisfy isDigit
            return sprintf "%s.%s" whole fraction
                |> Decimal.Parse
        } |> attempt

module private BigInt =

    /// Parses a positive big integer, ignoring underscores.
    let parse =
        many1Satisfy (fun c -> isDigit c || c = '_')
            |>> (fun str -> str.Replace("_", ""))
            |>> bigint.Parse

module private BigRational =

    /// Parses a positive exact exponential as a rational. E.g. "1.23ee45".
    let parseExactExponential =

        let parseBase =
            choice [
                Decimal.parse |>> BigRational.FromDecimal
                BigInt.parse |>> BigRational.FromBigInt
            ]

        parse {
            let! n = parseBase
            do! skipString "ee"
            let! exp = pint32
            return n * (10N ** exp)
        } |> attempt

    /// Parses a positive decimal number as a rational. E.g. "1.23", "1.23e-4".
    let parseDecimal =
        parse {
            let! n = Decimal.parse |>> BigRational.FromDecimal
            let! eOpt = opt (skipChar 'e')
            let! exp =
                if eOpt.IsSome then pint32
                else preturn 1
            return n * (10N ** exp)
        }

    /// Parses a fraction (or whole number) as a rational.
    let parseFraction =

            // parse entire denominator or leave slash unparsed (e.g. "1/unit")
        let parseDen =
            skipChar '/'
                >>. BigInt.parse
                |> attempt

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

    /// Parses an identifier.
    let identifier =
        let isAsciiIdStart c =
            isLetter c || c = '\\'
        let isAsciiIdContinue c =
            isAsciiIdStart c || isDigit c || c = '_'
        choice [
            IdentifierOptions(
                isAsciiIdStart,
                isAsciiIdContinue)
                |> identifier
            pstring "<<IMAGINARY_UNIT>>"   // ick
        ]

    /// Skips until the given string.
    let skipTill str =
        skipCharsTillString str true Int32.MaxValue

    /// Skips a comment.
    let skipComment =
        choice [
            skipString "//" .>> skipRestOfLine false   // a comment like this
            skipString "/*" >>. skipTill "*/"          // /* a comment like this */
        ]

    let skipFunction =

        let skipBody =
            parse {
                do! skipString "{"
                do! skipTill "}"
            }

        parse {
            do! identifier |>> ignore
            do! skipString "["
            do! skipTill ":="
            do! skipRestOfLine true
            do! optional skipBody
        } |> attempt

    /// Skips irrelevant text.
    let skipJunk =
        choice [
            spaces1   // whitespace, including newlines
            skipComment
            skipFunction
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
            choice [
                skipChar '+' >>% Unit.one
                skipChar '-' >>% (-1 @@ Unit.one)
                BigRational.parse |>> Unit.createScale
            ]

        /// Parses a term in an expression.
        let parseTerm =
            choice [
                parseDimensionless
                parseRef
            ]

        /// Parses an explicit power.
        let parsePowerExplicit =
            parse {
                do! skipChar '^'
                do! spaces
                return! pint32 // BigRational.parse
            } |> attempt

        /// Parses an explicit or implicit power.
        let parsePower =
            parsePowerExplicit <|>% 1

        /// Parses a term followed by a (possibly implicit) power.
        /// E.g. "m^2".
        let parseTermPower =
            parse {
                let! unit = parseTerm
                do! spaces
                let! power = parsePower
                return unit ^ power
            } |> attempt

        /// Parses the product of one or more term-powers. E.g. "m s^-1".
        let parseTermPowerProduct =
            many1 (parseTermPower .>> spaces)   // note: consumes trailing spaces
                |>> List.fold (*) Unit.one

        /// Parses a unified product followed by a (possibly implicit)
        /// power. E.g. "(hbar c / G)^(1/2)"
        let parseUnifiedProduct =
            parse {
                do! skipChar '('
                do! spaces
                let! unit = parseTermPowerProduct
                do! spaces
                do! skipChar ')'
                do! spaces
                let! power = parsePower
                return unit ^ power
            }

        /// Parses a unified expression - something that can be the denominator
        /// of a quotient.
        let parseUnified =
            choice [
                parseUnifiedProduct
                parseTermPower
            ]

        /// Parses a quotient. E.g. "dyne cm  / (abamp sec)".
        let parseQuotient =

            let parsePair =
                parse {
                    let! num = parseTermPowerProduct <|> parseUnified
                    do! spaces
                    do! skipChar '/'
                    do! spaces
                    let! den = parseUnified
                    return num, den
                } |> attempt

            parse {
                let! num, den = parsePair
                return!   // no Combine method on computation builder
                    if den.Scale = 0N then
                        failf "Denominator is zero: (%A)/(%A)" num den
                    else
                        preturn (num/den)
            }

        /// Parses an expression.
        let parseExpr =
            choice [
                parseQuotient
                parseTermPowerProduct
                parseUnified
            ]

        /// Parses the product of one or more expressions. E.g. "1/1000 kg".
        let parseExprProduct =
            many1 parseExpr
                |>> List.fold (*) Unit.one

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
            | Success ((), state', _) -> state'.Units, None
            | Failure (msg, _, state') -> state'.Units, Some msg

module Frink =

    /// Parses the given Frink file.
    let parseFile path =
        File.ReadAllText path |> FrinkParser.parse
