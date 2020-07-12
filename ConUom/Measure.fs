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

    let rec simplify measure =
        match measure.Unit with
            | DerivedUnit (unit, convert, _) ->
                simplify {
                    Value = convert |> Expr.eval measure.Value
                    Unit = unit
                }
            | _ -> measure

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
