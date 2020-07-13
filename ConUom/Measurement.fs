namespace ConUom

open MathNet.Numerics

/// An individual measurement. E.g. 3 lbs.
[<StructuredFormatDisplay("{String}")>]
type Measurement =
    {
        Value : BigRational
        Unit : Unit
    }

    /// Display string.
    member this.String =
        sprintf "%A %A" this.Value this.Unit

module Measurement =

    /// Creates a measurement.
    let inline create value unit =
        {
            Value = value
            Unit = unit
        }

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

    /// Builds an expression that simplifies the given unit.
    let private build unit =

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

    /// Simplifies the given measure. E.g. 2 in -> 5.08 cm
    let private simplify meas =

        let expr, unit = build meas.Unit

        create
            (expr |> Expr.eval meas.Value)
            unit

    /// Converts the given measurement to the given unit.
    let convert unit meas =

            // reduce measurement to base units
        let meas' = simplify meas

            // build conversion expression
        let expr, unit' = build unit
        if unit' <> meas'.Unit then
            failwithf "'%A' and '%A' are incompatible units" unit' meas.Unit
        let expr' = Expr.invert expr

            // convert value to new unit
        create
            (expr' |> Expr.eval meas'.Value)
            unit

    /// Multiplies two measurements. E.g. 10 ft * 12 ft = 120 ft^2.
    let product measA measB =
        create
            (measA.Value * measB.Value)
            (Unit.product measA.Unit measB.Unit)

[<AutoOpen>]
module MeasureAutoOpen =

    /// Creates a measurement.
    let inline (@) value unit =
        Measurement.create value unit


/// https://stackoverflow.com/questions/2812084/overload-operator-in-f/2812306#2812306
/// http://nut-cracker.azurewebsites.net/blog/2011/10/05/inlinefun/
type ProductExt =
    | ProductExt

    /// Normal arithmetic product.
    static member inline (=>) (a, _ : ProductExt) =
        fun b -> a * b

    /// Unit product.
    static member (=>) (unitA, _ : ProductExt) =
        fun unitB -> Unit.product unitA unitB

    /// Measurement product.
    static member (=>) (measA, _ : ProductExt) =
        fun measB -> Measurement.product measA measB

    /// Dummy member to create ambiguity between the overloads.
    static member (=>) (_ : ProductExt, _ : ProductExt) =
        failwith "Unexpected"
        id<ProductExt>

[<AutoOpen>]
module ProductExt2 =

    /// Extended product operator.
    let inline (*) a b =
        (a => ProductExt) b
