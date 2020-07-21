namespace ConUom

open System
open MathNet.Numerics

/// A unit of measurement.
[<StructuredFormatDisplay("{Name}")>]
type Unit =
    private {

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

    /// Creates a new unit based on the given unit.
    let create unit scale =
        {
            BaseMap = unit.BaseMap
            Scale = scale * unit.Scale
        }

    /// Creates a dimensionless unit.
    let createScale scale =
        {
            BaseMap = Map.empty
            Scale = scale
        }

    /// Answers the given unit's set of base units.
    let baseUnits unit =
        unit.BaseMap
            |> Map.toSeq
            |> Set

    /// Answers the given unit's scale.
    let scale unit =
        unit.Scale

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

    /// Dimensionless unit of scale one.
    let one =
        createScale 1N

    /// Raises a unit to a power.
    let (^) unit power =
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

    /// Creates a new unit based on the given unit.
    static member (@@)(scale, unit) =
        scale |> Unit.create unit

    /// Creates a new unit based on the given unit.
    static member (@@)(scale, unit) =
        BigRational.FromDecimal(scale) |> Unit.create unit

    /// Creates a new unit based on the given unit.
    static member (@@)(scale, unit) =
        BigRational.FromInt(scale) |> Unit.create unit

    /// Raises a unit to a power.
    static member Pow(unit, power) =
        Unit.(^) unit power

[<AutoOpen>]
module UnitExt =

    /// Raises a unit to an integer power.
    let (^) unit power =
        Unit.(^) unit (BigRational.FromInt power)
