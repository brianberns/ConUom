namespace ConUom

open System
open MathNet.Numerics

/// A unit of measurement.
[<StructuredFormatDisplay("{Name}")>]
type Unit =
    {
        /// Base units that this unit derives from. E.g. Units
        /// of acceleration are based on: m^1, s^-2.
        BaseMap : Map<BaseUnit, int (*power*)>

        /// Factor to convert from this unit to base units. E.g.
        /// 1000x for km -> m.
        Scale : BigRational
    }

    /// Name of this unit.
    member this.Name =
        let scaleStr = sprintf "%A" <| float this.Scale
        if this.BaseMap.IsEmpty then
            scaleStr
        else
            let units =
                let names =
                    this.BaseMap
                        |> Map.toSeq
                        |> Seq.map (fun (baseUnit, power) ->
                            let name = baseUnit.Name
                            if power = 1 then name
                            else sprintf "%s^%d" name power)
                String.Join(" ", names)
            if this.Scale = 1N then
                units
            else
                sprintf "%s %s" scaleStr units

module Unit =

    /// Creates a base unit.
    let createBase dim name =
        {
            BaseMap =  Map [ BaseUnit.create dim name, 1 ]
            Scale = 1N
        }

    /// Creates a dimensionless unit of the given scale.
    let fromScale scale =
        {
            BaseMap = Map.empty
            Scale = scale
        }

    /// Dimensionless unit of scale one.
    let one =
        fromScale 1N

    /// Answers the given unit's set of base units.
    let baseUnits unit =
        unit.BaseMap
            |> Map.toSeq
            |> Set

    /// Adds two units.
    let add unitA unitB =
        if unitA.BaseMap <> unitB.BaseMap then
            failwithf "Can't add '%A' to '%A'" unitA unitB
        {
            unitA with
                Scale = unitA.Scale + unitB.Scale
        }

    /// Subtracts one unit from another.
    let sub unitA unitB =
        unitA + (-unitB)

    /// Multiplies two units.
    let mult unitA unitB =
        let baseMap =
            (unitA.BaseMap, unitB.BaseMap |> Map.toSeq)
                ||> Seq.fold (fun baseMap (baseUnit, power) ->
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
        {
            BaseMap = baseMap
            Scale = unitA.Scale * unitB.Scale
        }

    /// Inverts a unit. E.g. ft^2 -> ft^-2.
    let invert unit =
        if unit.Scale = 0N then
            failwithf "Can't invert unit: %A" unit
        {
            BaseMap =
                unit
                    |> baseUnits
                    |> Seq.map (fun (baseUnit, power) ->
                        baseUnit, -power)
                    |> Map
            Scale = 1N / unit.Scale
        }

    /// Divides one unit by another.
    let div numUnit denUnit =
        mult numUnit (invert denUnit)

    /// Raises a unit to a rational power.
    let pow power unit =
        if power = 0N then   // a^0 -> 1
            one
        else

            let baseMap =
                unit.BaseMap
                    |> Map.toSeq
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

            {
                BaseMap = baseMap
                Scale = scale
            }

type Unit with

    /// Negates a unit.
    static member(~-)(unit) =
        { unit with Scale = -unit.Scale }

    /// Adds two units.
    static member(+)(unitA, unitB) =
        Unit.add unitA unitB

    // Subtracts one unit from another.
    static member(-)(unitA, unitB) =
        Unit.sub unitA unitB

    /// Multiplies two units.
    static member (*)(unitA, unitB) =
        Unit.mult unitA unitB

    /// Scales a unit.
    static member (*)(unit, scale) =
        Unit.mult unit (Unit.fromScale scale)

    /// Scales a unit.
    static member (*)(scale, unit) =
        Unit.mult (Unit.fromScale scale) unit

    /// Scales a unit.
    static member (*)(unit, scale) =
        Unit.mult unit (scale |> BigRational.FromInt |> Unit.fromScale)

    /// Scales a unit.
    static member (*)(scale, unit) =
        Unit.mult (scale |> BigRational.FromInt |> Unit.fromScale) unit

    /// Scales a unit.
    static member (*)(unit, scale) =
        Unit.mult unit (scale |> BigRational.FromDecimal |> Unit.fromScale)

    /// Scales a unit.
    static member (*)(scale, unit) =
        Unit.mult (scale |> BigRational.FromDecimal |> Unit.fromScale) unit

    /// Divides one unit by another.
    static member (/)(unitA, unitB) =
        Unit.div unitA unitB

    /// Scales a unit.
    static member (/)(unit, scale) =
        Unit.div unit (Unit.fromScale scale)

    /// Scales a unit.
    static member (/)(scale, unit) =
        Unit.div (Unit.fromScale scale) unit

    /// Scales a unit.
    static member (/)(unit, scale) =
        Unit.div unit (scale |> BigRational.FromInt |> Unit.fromScale)

    /// Scales a unit.
    static member (/)(scale, unit) =
        Unit.div (scale |> BigRational.FromInt |> Unit.fromScale) unit

    /// Scales a unit.
    static member (/)(unit, scale) =
        Unit.div unit (scale |> BigRational.FromDecimal |> Unit.fromScale)

    /// Scales a unit.
    static member (/)(scale, unit) =
        Unit.div (scale |> BigRational.FromDecimal |> Unit.fromScale) unit

    /// Raises a unit to a rational power. This is invoked via ** rather than ^.
    static member Pow(unit, power) =
        Unit.pow power unit

    /// Raises a unit to an integer power. This is invoked via ** rather than ^.
    static member Pow(unit, power : int) =
        unit ** (BigRational.FromInt power)

[<AutoOpen>]
module UnitExt =

    /// Raises a unit to an integer power.
    let (^) (unit : Unit) (power : int) =
        unit ** power
