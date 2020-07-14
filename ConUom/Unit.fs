﻿namespace ConUom

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

    /// This unit's set of base units.
    member this.BaseUnits =
        this.BaseMap
            |> Map.toSeq
            |> Set

    /// Name of this unit.
    member this.Name =
        if this.BaseMap.IsEmpty then
            sprintf "%A" this.Scale
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
            sprintf "%A @@ %s" this.Scale units

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
        {
            BaseMap =
                (unit : Unit).BaseUnits
                    |> Seq.map (fun (baseUnit, power) ->
                        baseUnit, -power)
                    |> Map
            Scale = 1N / unit.Scale
        }

    /// Divides one unit by another.
    let div numUnit denUnit =
        mult numUnit (invert denUnit)

    /// Raises a unit to a power.
    let (^) unit power =
        if power = 0 then   // a^0 -> 1
            createScale 1N
        else
            let baseMap =
                unit.BaseMap
                    |> Map.toSeq
                    |> Seq.map (fun (baseUnit, oldPower) ->
                        assert(oldPower <> 0)
                        baseUnit, oldPower * power)   // (a^n)^m -> a^(n*m)
                    |> Map
            {
                BaseMap = baseMap
                Scale = unit.Scale ** power
            }

/// https://stackoverflow.com/questions/2812084/overload-operator-in-f/2812306
/// http://nut-cracker.azurewebsites.net/blog/2011/10/05/inlinefun/

/// Unit creation operator.
type UnitExt =
    | UnitExt

    /// Create unit from rational.
    static member (=>) (scale, _ : UnitExt) =
        fun unit -> Unit.create unit scale

    /// Create unit from decimal.
    static member (=>) (scale, _ : UnitExt) =
        fun unit -> Unit.create unit (BigRational.FromDecimal scale)

    /// Create unit from integer.
    static member (=>) (scale, _ : UnitExt) =
        fun unit -> Unit.create unit (BigRational.FromInt scale)

    /// Dummy member to create ambiguity between the overloads.
    static member (=>) (_ : UnitExt, _ : UnitExt) =
        failwith "Unexpected"
        id<UnitExt>

[<AutoOpen>]
module UnitExt =

    /// Creates a unit.
    let inline (@@) a b =
        (a => UnitExt) b

    /// Raises a unit to a power.
    let (^) = Unit.(^)
