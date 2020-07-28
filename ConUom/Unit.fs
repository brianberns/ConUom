namespace ConUom

open System
open System.Collections.Immutable

open MathNet.Numerics

/// E.g. "m" for meter or "s" for second.
type BaseUnit = string

/// A unit of measurement.
[<StructuredFormatDisplay("{String}")>]
type Unit private (baseMap, scale) =

    /// Creates a base unit.
    new(name) =
        let baseMap = Map [ name, 1 ]
        Unit(baseMap, 1N)

    /// Creates a dimensionless unit of the given scale.
    new(scale) =
        Unit(Map.empty, scale)

    /// Creates a dimensionless unit of the given scale.
    new(scale) =
        Unit(scale |> BigRational.FromInt)

    /// Creates a dimensionless unit of the given scale.
    new(scale) =
        Unit(scale |> BigRational.FromDecimal)

    /// Base units that this unit derives from. E.g. Units
    /// of acceleration are based on: m^1, s^-2.
    member internal __.BaseMap : Map<BaseUnit, int (*power*)> =
        baseMap

    /// Factor to convert from this unit to base units. E.g.
    /// 1000x for km -> m.
    member __.Scale : BigRational =
        scale

    /// Answers the given unit's set of base units.
    member unit.BaseUnits =
        unit.BaseMap
            |> Map.toSeq
            |> ImmutableHashSet.ToImmutableHashSet

    /// Dimensionless unit of scale one.
    static member One =
        Unit(1N)

    /// Display string.
    member private unit.String =
        let scaleStr = sprintf "%A" <| float unit.Scale
        if unit.BaseMap.IsEmpty then
            scaleStr
        else
            let units =
                let names =
                    unit.BaseMap
                        |> Seq.map (fun (KeyValue(baseUnit, power)) ->
                            if power = 1 then baseUnit
                            else sprintf "%s^%d" baseUnit power)
                String.Join(" ", names)
            if unit.Scale = 1N then
                units
            else
                sprintf "%s %s" scaleStr units

    /// Display string.
    override unit.ToString() =
        unit.String

    /// Negates a unit.
    static member (~-)(unit : Unit) =
        Unit(unit.BaseMap, -unit.Scale)

    /// Adds two units.
    static member (+)(unitA : Unit, unitB : Unit) =
        if unitA.BaseMap <> unitB.BaseMap then
            failwithf "Can't add '%A' to '%A'" unitA unitB
        Unit(unitA.BaseMap, unitA.Scale + unitB.Scale)

    /// Subtracts one unit from another.
    static member (-)(unitA : Unit, unitB : Unit) =
        unitA + (-unitB)

    /// Multiplies two units.
    static member (*)(unitA : Unit, unitB : Unit) =
        let baseMap =
            (unitA.BaseMap, unitB.BaseMap)
                ||> Seq.fold (fun baseMap (KeyValue(baseUnit, power)) ->
                    let oldPower =
                        baseMap
                            |> Map.tryFind baseUnit
                            |> Option.defaultValue 0
                    let newPower = oldPower + power   // (a^n)(a^m) -> a^(n+m)
                    if newPower = 0 then
                        baseMap
                            |> Map.remove baseUnit
                    else
                        baseMap
                            |> Map.add baseUnit (oldPower + power))
        assert(
            baseMap
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.forall ((<>) 0))
        Unit(baseMap, unitA.Scale * unitB.Scale)

    /// Scales a unit.
    static member (*)(unit, scale : BigRational) =
        unit * Unit(scale)

    /// Scales a unit.
    static member (*)(scale : BigRational, unit) =
        Unit(scale) * unit

    /// Scales a unit.
    static member (*)(unit, scale :int) =
        unit * Unit(scale)

    /// Scales a unit.
    static member (*)(scale : int, unit) =
        Unit(scale) * unit

    /// Scales a unit.
    static member (*)(unit, scale : decimal) =
        unit * Unit(scale)

    /// Scales a unit.
    static member (*)(scale : decimal, unit) =
        Unit(scale) * unit

    /// Divides one unit by another.
    static member(/)(unitA : Unit, unitB : Unit) =

        /// Inverts a unit. E.g. ft^2 -> ft^-2.
        let invert (unit : Unit) =
            if unit.Scale = 0N then
                failwithf "Can't invert unit: %A" unit
            let baseMap =
                unit.BaseUnits
                    |> Seq.map (fun (baseUnit, power) ->
                        baseUnit, -power)
                    |> Map
            Unit(baseMap, 1N / unit.Scale)

        unitA * (invert unitB)

    /// Scales a unit.
    static member (/)(unit, scale : BigRational) =
        unit / Unit(scale)

    /// Scales a unit.
    static member (/)(scale : BigRational, unit) =
        Unit(scale) / unit

    /// Scales a unit.
    static member (/)(unit, scale : int) =
        unit / Unit(scale)

    /// Scales a unit.
    static member (/)(scale : int, unit) =
        Unit(scale) / unit

    /// Scales a unit.
    static member (/)(unit, scale : decimal) =
        unit / Unit(scale)

    /// Scales a unit.
    static member (/)(scale : decimal, unit) =
        Unit(scale) / unit

    /// Raises a unit to a rational power.
    static member Pow(unit : Unit, power) =
        if power = 0N then   // a^0 -> 1
            Unit.One
        else

            let baseMap =
                unit.BaseUnits
                    |> Seq.map (fun (baseUnit, oldPower) ->
                        assert(oldPower <> 0)
                        let newPower =   // (a^n)^m -> a^(n*m)
                            let newPower = (BigRational.FromInt oldPower) * power
                            if newPower.IsInteger then
                                int newPower
                            else
                                failwithf "Invalid power: (%A)^%A" unit power
                        baseUnit, newPower)
                    |> Map

            let scale =
                if power.IsInteger then
                    unit.Scale ** int power
                else
                    Math.Pow(float unit.Scale, float power)   // have to use floating point :(
                        |> decimal
                        |> BigRational.FromDecimal

            Unit(baseMap, scale)

    /// Raises a unit to an integer power. This is invoked via ** rather than ^.
    static member Pow(unit, power : int) =
        unit ** (BigRational.FromInt power)

    /// Raises a unit to an integer power. This can be invoked via ^ from C#.
    static member op_ExclusiveOr(unit : Unit, power : int) =
        unit ** (BigRational.FromInt power)

[<AutoOpen>]
module UnitExt =

    /// Raises a unit to an integer power. (Note that this blocks
    /// use of ^ for string concatenation.)
    let (^) (unit : Unit) (power : int) =
        unit ** power
