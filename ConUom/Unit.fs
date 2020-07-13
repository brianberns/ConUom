namespace ConUom

open System
open MathNet.Numerics

type BaseDimension =
    | Dimensionless
    | Length
    | Mass

type CoreUnit =
    {
        BaseDimension : BaseDimension

        /// Factor to convert from scaled unit to base unit. E.g. 1000x for km -> m.
        Scale : BigRational

        Name : string
    }

module CoreUnit =

    let createBase dim name =
        {
            BaseDimension = dim
            Scale = 1N
            Name = name
        }

    let create core scale name =
        {
            BaseDimension = core.BaseDimension
            Scale = scale * core.Scale
            Name = name
        }

[<StructuredFormatDisplay("{Name}")>]
type Unit =
    {
        CoreMap : Map<CoreUnit, int>
    }

    /// Name of this unit.
    member this.Name =
        if this.CoreMap.IsEmpty
            then "1"
        else
            let names =
                this.CoreMap
                    |> Map.toSeq
                    |> Seq.map (fun (core, power) ->
                        let name = core.Name
                        if power = 1 then name
                        else sprintf "(%s)^%d" name power)
            String.Join(" ", names)

module Unit =

    let one = { CoreMap = Map.empty }

    let private add core power unit =
        let oldPower =
            unit.CoreMap
                |> Map.tryFind core
                |> Option.defaultValue 0
        {
            CoreMap =
                unit.CoreMap
                    |> Map.add core (oldPower + power)
        }

    let rec normalize unit : Unit =

            // a^0 -> 1
        if unit.Power = 0 then
            one
        else
            match unit.Core with

                | BaseUnit _ -> unit

                | ProductUnit (unitA, unitB) ->

                        // (a^n)(a^m) -> a^(n+m)
                    if unit.Power = 1 then
                        let unitA' = normalize unitA
                        let unitB' = normalize unitB
                        if unitA'.Core = unitB'.Core then
                            create unitA'.Core (unitA'.Power + unitB'.Power)
                                |> normalize
                        else
                            ProductUnit (unitA', unitB') |> ofCore

                        // (ab)^n -> (a^n)(b^n)
                    else
                        let unitA' = unitA ^ unit.Power
                        let unitB' = unitB ^ unit.Power
                        ProductUnit (unitA', unitB') |> ofCore

                | DerivedUnit (unit, expr, name) ->
                    create
                        (DerivedUnit (normalize unit, expr, name))
                        unit.Power

    and (^) unit power =

            // (a^n)^m -> a^(n*m)
        create
            unit.Core
            (unit.Power * power)
            |> normalize

    /// An integer power of a rational base.
    let rec private exp x y =
        match y with
            | 0 -> 1N
            | 1 -> x
            | _ ->
                if y > 1 then
                    x * (exp x (y - 1))
                else
                    failwith "Unexpected"

    /// Creates an expression that simplifies the given unit.
    let simplify unit =

        let rec loop outer = function

            | DerivedUnit (unit, inner, _) ->
                loop (Expr.subst outer inner) unit

                // E.g. ft^2 = 2.54^2 cm^2
            | PowerUnit (DerivedUnit (unit, inner, _), power) ->
                let expr =
                    match inner with
                        | Product (Const x, Var)
                        | Product (Var, Const x) ->
                            Product (Const (exp x power), Var)
                        | Quotient (Var, Const x) ->
                            Quotient (Var, Const (exp x power))
                        | _ -> failwith "Unexpected"
                        |> Expr.subst outer
                let unit' =
                    PowerUnit (unit, power)
                loop expr unit'

            | unit -> outer, unit

        loop Var unit

    let product unitA unitB =
        ProductUnit (unitA, unitB)
            |> ofCore
            |> normalize

[<AutoOpen>]
module UnitAutoOpen =
    let (^) = Unit.(^)
