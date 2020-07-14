namespace ConUom

open System
open MathNet.Numerics

type BaseDimension =
    | Dimensionless
    | Length
    | Mass
    | Time

type BaseUnit =
    {
        BaseDimension : BaseDimension
        Name : string
    }

module BaseUnit =

    let create dim name =
        {
            BaseDimension = dim
            Name = name
        }

[<StructuredFormatDisplay("{Name}")>]
type Unit =
    {
        BaseMap : Map<BaseUnit, int (*power*)>

        /// Factor to convert from outer unit to base units. E.g. 1000x for km -> m.
        Scale : BigRational

        Name : string
    }

    member this.BaseUnits =
        this.BaseMap
            |> Map.toSeq
            |> Set

module Unit =

    let one =
        {
            BaseMap = Map.empty
            Scale = 1N
            Name = "1"
        }

    let createBase dim name =
        {
            BaseMap =  Map [ BaseUnit.create dim name, 1 ]
            Scale = 1N
            Name = name
        }

    let create unit scale name =
        {
            BaseMap = unit.BaseMap
            Scale = scale * unit.Scale
            Name = name
        }

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
            Name = sprintf "%s %s" unitA.Name unitB.Name
        }

    let invert unit =
        {
            BaseMap =
                unit.BaseMap
                    |> Map.toSeq
                    |> Seq.map (fun (baseUnit, power) ->
                        baseUnit, -power)
                    |> Map
            Scale = 1N / unit.Scale
            Name = sprintf "1/%s" unit.Name
        }

    let div unitA unitB =
        {
            mult unitA (invert unitB) with
                Name = sprintf "%s/%s" unitA.Name unitB.Name
        }

    let (^) unit power =
        if power = 0 then one   // a^0 -> 1
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
                Name = sprintf "%s^%d" unit.Name power
            }

[<AutoOpen>]
module UnitAutoOpen =
    let (^) = Unit.(^)
