﻿namespace ConUom

open System
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

open MathNet.Numerics

open FParsec

/// State maintained by the Frink parser.
type UnitLookup =
    {
        /// Prefixes. E.g. "milli" = 1/1000.
        Prefixes : Map<string, BigRational>

        /// Units. E.g. "gram" = 1/1000 kg.
        Units : Map<string, Unit>
    }

    /// Tries to find the prefix with the given name.
    member lookup.TryFindPrefix(name)=
        lookup.Prefixes
            |> Map.tryFind name

    /// Tries to find the unit with the given name.
    member lookup.TryFindUnit(name) =

        let tryFind name =
            lookup.Units
                |> Map.tryFind name
                |> Option.orElseWith (fun () ->

                        // try prefixes if name not found
                    lookup.Prefixes
                        |> Map.toSeq
                        |> Seq.tryPick (fun (prefix, scale) ->
                            if name.StartsWith(prefix) then
                                let name' =
                                    name.Substring(prefix.Length)
                                lookup.Units
                                    |> Map.tryFind name'
                                    |> Option.map (fun unit ->
                                        scale * unit)
                            else None))

        tryFind name

                // try converting plural to singular
            |> Option.orElseWith (fun () ->
                if name.EndsWith('s') then
                    tryFind (name.Substring(0, name.Length-1))
                else None)

                // try a prefix instead
            |> Option.orElseWith (fun () ->
                lookup.TryFindPrefix(name)
                    |> Option.map Unit)

    /// Tries to find the unit with the given name.
    member lookup.Item
        with get(name) =
            let unitOpt = lookup.TryFindUnit(name)
            match unitOpt with
                | Some unit -> unit
                | _ -> failwithf "No such unit: %s" name

#if DEBUG
[<AutoOpen>]
module private Debug =

    /// Debug parser.
    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply
#endif

module private Decimal =

    /// Parses a simple positive decimal number. Must include
    /// a decimal point. E.g. "1.23".
    let parse =
        parse {
            let! whole = manySatisfy isDigit
            do! skipChar '.'
            let! fraction = manySatisfy isDigit
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

    /// Parses a simple number as a rational.
    let parseSimple =
        choice [
            Decimal.parse |>> BigRational.FromDecimal
            BigInt.parse |>> BigRational.FromBigInt
        ]

    /// Parses a positive fraction as a rational. E.g. "1/2".
    let parseFraction =
        parse {
            let! num = parseSimple
            do! skipChar '/'
            let! den = parseSimple
            return num / den
        } |> attempt

    /// Parses a positive number as a rational. E.g. "1", "1e2", "1.23", "1.23e-4".
    let parseDecimal =
        parse {
            let! n = parseSimple
            let! exp =
                ((skipString "ee" <|> skipString "e") >>. pint32)
                    <|>% 0
            return n * (10N ** exp)
        } |> attempt

    /// Parses a signed value.
    let parseSigned parser =
        parse {
            let! hyphenOpt = opt (skipChar '-')
            let sign = if hyphenOpt.IsNone then 1N else -1N
            let! value = parser
            return sign * value
        }

    /// Parses a rational.
    let parse =
        choice [
            parseFraction
            parseDecimal
        ] |> parseSigned

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

    module Expr =

        /// Parses a reference to a unit.
        let parseRef =
            parse {
                let! name = identifier
                let! (lookup : UnitLookup) = getUserState
                let unitOpt = lookup.TryFindUnit(name)
                return!
                    match unitOpt with
                        | Some unit -> preturn unit
                        | None -> failf "Unknown unit: %s" name
            } |> attempt

        /// Parses a dimensionless unit.
        let parseDimensionless =
            BigRational.parse |>> Unit

        /// Parses a term in an expression.
        let parseTerm =
            choice [
                parseDimensionless
                parseRef
            ]

        /// Requires parentheses around the given parser.
        let parseParenthesized parser =
            parse {
                do! skipChar '('
                let! value = parser
                do! skipChar ')'
                return value
            } |> attempt

        /// Accepts (but doesn't require) parentheses around the given
        /// parser.
        let acceptParenthesized parser =
            choice [
                parser
                parseParenthesized parser
            ]

        /// Parses an explicit power. E.g. "^2".
        let parsePower =
            parse {
                do! skipChar '^'
                do! spaces
                return! acceptParenthesized BigRational.parse
            } |> attempt

        /// Accepts (but doesn't require) a power after the given parser.
        let acceptPower parser =
            parse {
                let! value = parser
                do! spaces
                let! power = parsePower <|>% 1N
                return value ** power
            } |> attempt

        /// Parses a term followed by a (possibly implicit) power.
        /// E.g. "m^2".
        let parseTermPower =
            acceptPower parseTerm

        /// Backtracks if the given parser fails or if its result doesn’t
        /// satisfy a predicate function. Based on:
        /// https://www.quanttec.com/fparsec/users-guide/looking-ahead-and-backtracking.html#parser-predicates
        let resultSatisfies predicate (p: Parser<_,_>) : Parser<_,_> =
            fun stream ->
                let state = stream.State
                let reply = p stream
                if reply.Status <> Ok then
                    stream.BacktrackTo(state)
                    reply
                elif predicate reply.Result then
                    reply
                else
                    stream.BacktrackTo(state) // backtrack to beginning
                    Reply(Error, NoErrorMessages)

        /// Parses 2 or more occurrences.
        let require2 =
            resultSatisfies (fun items ->
                List.length items >= 2)

        /// Parses two or more occurrences.
        let sepBy2 p sep =
            sepBy1 p sep |> require2

        /// Parses two or more occurrences.
        let many2 p =
            many1 p |> require2

        /// Parses the sum of one or more units.
        let parseUnitSum parseUnit =
            let skipOp =
                spaces >>. skipChar '+' .>> spaces
            sepBy2 parseUnit skipOp
                |>> List.reduce (+)
            
        /// Parses the difference of two units.
        let parseUnitDiff parseUnit =
            parse {
                let! unitA = parseUnit
                do! spaces
                do! skipChar '-'
                do! spaces
                let! unitB = parseUnit
                return unitA - unitB
            } |> attempt

        /// Parses the product of one or more units.
        let parseUnitProduct parseUnit =
            let skipOp =
                spaces
                    >>. optional (skipChar '*')
                    .>> spaces
            many2 (parseUnit .>> skipOp)   // note: consumes trailing spaces
                |>> List.fold (*) Unit.One

        /// Parses the sum of one or more terms. E.g. "(365 + 1/4)".
        let parseTermSum =
            choice [
                parseUnitSum parseTerm
                parseUnitDiff parseTerm
            ]

        /// Parses the product of one or more term-powers. E.g. "m s^-1".
        let parseTermPowerProduct =
            parseUnitProduct parseTermPower

        /// Parses a potential numerator or denominator. E.g. "(m s^-1)"
        /// or "kg", but not "m s^-1".
        let parseQuotientable =
            choice [
                choice [
                    parseTermPowerProduct
                    parseTermSum
                ] |> parseParenthesized
                parseTermPower
            ]

        /// Parses a quotient.
        let parseQuotient =

            let inner parser =
                parse {
                    let! num = parser
                    do! spaces
                    do! skipChar '/'
                    do! spaces
                    let! den = parseQuotientable
                    return num / den
                } |> attempt

            choice [
                inner (inner parseQuotientable)   // e.g. "km/s/megaparsec"
                inner parseQuotientable           // e.g. "km/s"
            ]

        /// Parses a potential multiplicand. E.g. "m", "m^2", or "m/s".
        let parseMultiplicand =
            choice [
                parseQuotient
                parseQuotientable
            ] |> acceptParenthesized

        /// Parses a product. E.g. "1200/3937 m/ft".
        let parseProduct =
            choice [
                parseUnitProduct parseMultiplicand
                parseMultiplicand
            ]

        /// Parses a potentially signed unit.
        let acceptSigned parser =
            parse {
                let! charOpt = opt (anyOf [ '-'; '+' ] .>> spaces)
                let sign =
                    if charOpt = Some '-' then -1N
                    else 1N
                let! unit = parser
                return Unit.(*)(sign, unit)
            }

        /// Parses a sum. E.g. "26 miles + 385 yards".
        let parseSum =
            choice [
                parseUnitSum parseProduct
                parseProduct
            ]

        /// Parses an expression.
        let parse =
            parseSum
                |> acceptParenthesized
                |> acceptPower
                |> acceptSigned

    module Prefix =

        /// Adds the given prefix to the given lookup.
        let add lookup (name, scale) =
            { lookup with
                Prefixes = lookup.Prefixes |> Map.add name scale }

        /// Parses a dimensionless scale.
        let parseScale =
            parse {
                let! unit = Expr.parse
                if unit.BaseUnits.IsEmpty then
                    return unit.Scale
                else
                    return! failf "Not a scale: %A" unit
            }                    

        /// Parses a prefix reference.
        let parseRef =
            parse {
                let! name = identifier
                let! (lookup : UnitLookup) = getUserState
                let scaleOpt = lookup.TryFindPrefix(name)
                return!
                    match scaleOpt with
                        | Some scale -> preturn scale
                        | None -> failf "Unknown prefix: %s" name
            } |> attempt

        let parseScaleOrRef =
            choice [
                parseScale
                parseRef
            ]

        /// Parses a prefix declaration.
        let parseDecl =
            parse {
                let! name = identifier
                do! spaces
                do! skipString "::-" <|> skipString ":-"
                do! spaces
                let! scale = parseScaleOrRef
                return name, scale
            } |> attempt

        /// Consumes a prefix declaration.
        let consumeDecl =
            consume parseDecl add

    module Unit =

        /// Adds the given unit to the given lookup.
        let add lookup (name, unit) =
            { lookup with
                Units = lookup.Units |> Map.add name unit }

        /// Parses the declaration of a base unit. E.g. "length =!= m".
        let parseBaseDecl =
            parse {
                let! dim = identifier   // not used
                do! spaces
                do! skipString "=!="
                do! spaces
                let! name = identifier
                return name, Unit(name)
            } |> attempt

        /// Parses the declaration of a derived unit.
        /// E.g. "kilogram := kg"
        /// E.g. "gram := 1/1000 kg"
        /// E.g. "radian := 1"
        /// E.g. "earthvolume = 4/3 pi ..." (using = instead of :=, for some reason)
        let parseDerivedDecl =
            parse {
                let! name = identifier
                do! spaces
                do! skipString ":=" <|> skipString "="
                do! spaces
                let! unit = Expr.parse
                return name, unit
            } |> attempt

        /// Parses the declaration of a combination unit.
        /// E.g. "m^2 K / W ||| thermal_insulance".
        let parseCombinationDecl =
            parse {
                let! unit = Expr.parse
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

module Frink =

    /// Parses the given Frink declarations. Note that this parser
    /// is just good enough to parse Frink's standard units.txt
    /// file, but is not a general-purpose Frink parser.
    let parse str =
        let parser = FrinkParser.consumeDecls .>> eof   // force consumption of entire string
        let str = if isNull str then "" else str
        let lookup =
            {
                Prefixes = Map.empty
                Units =
                    Map [
                        "<<IMAGINARY_UNIT>>", Unit.One   // ick
                    ]
            }
        match runParserOnString parser lookup "" str with
            | Success ((), lookup', _) -> lookup', None
            | Failure (msg, _, lookup') -> lookup', Some msg

    /// Parses the given Frink file.
    let parseFile path =
        File.ReadAllText path |> parse

[<AutoOpen>]
module FrinkAutoOpen =

    /// Dynamic unit lookup.
    let (?) (lookup : UnitLookup) name =
        lookup.TryFindUnit(name)
            |> Option.defaultWith (fun () ->
                failwithf "No such unit: %s" name)

/// C# API.
[<AbstractClass; Sealed>]   // appear as a static class to C# clients
type Frink =

    /// Tries to parse the given Frink declarations.
    static member TryParse(str, [<Out>] lookup : UnitLookup byref) =
        let result, msgOpt = Frink.parse str
        if msgOpt.IsNone then
            lookup <- result
            true
        else
            false
