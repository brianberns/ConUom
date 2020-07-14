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
    let create value unit =
        {
            Value = value
            Unit = unit
        }

    /// Converts the given measurement to the given unit.
    let convert unit meas =

        let baseUnits = (unit : Unit).BaseUnits
        if baseUnits <> meas.Unit.BaseUnits then
            failwithf "Can't convert '%s' to '%s'" meas.Unit.Name unit.Name
        let value =
            meas.Value * meas.Unit.Scale / unit.Scale
        create value unit

    /// Multiplies two measurements. E.g. 10 ft * 12 ft = 120 ft^2.
    let product measA measB =
        create
            (measA.Value * measB.Value)
            (Unit.mult measA.Unit measB.Unit)

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
        fun unitB -> Unit.mult unitA unitB

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
