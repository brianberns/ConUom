namespace ConUom

open MathNet.Numerics

[<StructuredFormatDisplay("{String}")>]
type Measure =
    {
        Value : BigRational
        Unit : Unit
    }

    member this.String =
        sprintf "%A %A" this.Value this.Unit

module Measure =

    let inline create value unit =
        {
            Value = value
            Unit = unit
        }

    let rec private exp x y =
        match y with
            | 0 -> 1N
            | 1 -> x
            | _ ->
                if y > 1 then
                    x * (exp x (y - 1))
                else
                    failwith "Unexpected"

    let rec build outer = function
        | DerivedUnit (unit, inner, _) ->
            build (Expr.subst outer inner) unit
        | PowerUnit (DerivedUnit (unit, inner, _), power) ->   // e.g. x ft^2 = 2.54^2*x cm^2
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
            build expr unit'
        | unit -> outer, unit

    let private simplify measure =

        let expr, unit = build Var measure.Unit

        create
            (expr |> Expr.eval measure.Value)
            unit

    let convert unit measure =

        let measure' = simplify measure

        let expr, unit' = build Var unit
        if unit' <> measure'.Unit then
            failwithf "'%A' and '%A' are incompatible units" unit' measure.Unit
        let expr' = Expr.invert expr

        create
            (expr' |> Expr.eval measure'.Value)
            unit

    let product measA measB =
        create
            (measA.Value * measB.Value)
            (Unit.product measA.Unit measB.Unit)

[<AutoOpen>]
module MeasureAutoOpen =

    let inline (@) value unit =
        Measure.create value unit


/// https://stackoverflow.com/questions/2812084/overload-operator-in-f/2812306#2812306
/// http://nut-cracker.azurewebsites.net/blog/2011/10/05/inlinefun/
type ProductExt =
    | ProductExt

    static member inline (=>) (a, _ : ProductExt) =
        fun b -> a * b

    static member (=>) (unitA, _ : ProductExt) =
        fun unitB -> Unit.product unitA unitB

    static member (=>) (measA, _ : ProductExt) =
        fun measB -> Measure.product measA measB

    /// Dummy member to create ambiguity between the overloads.
    static member (=>) (_ : ProductExt, _ : ProductExt) =
        failwith "Unexpected"
        id<ProductExt>

[<AutoOpen>]
module ProductExt2 =

    /// Extended product operator.
    let inline (*) a b =
        (a => ProductExt) b
