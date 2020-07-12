namespace ConUom

[<StructuredFormatDisplay("{String}")>]
type Measure =
    {
        Value : decimal
        Unit : Unit
    }

    member this.String =
        sprintf "%M %A" this.Value this.Unit

module Measure =

    let inline create value unit =
        {
            Value = decimal value
            Unit = unit
        }

    let simplify measure =

        let rec loop outer = function
            | DerivedUnit (unit, inner, _) ->
                loop (Expr.subst outer inner) unit
            | unit -> outer, unit

        let expr, unit = loop Var measure.Unit

        create
            (expr |> Expr.eval measure.Value)
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
