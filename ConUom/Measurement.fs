namespace ConUom

open MathNet.Numerics

/// An individual measurement. E.g. 3 lbs.
[<StructuredFormatDisplay("{String}")>]
type Measurement =
    {
        /// Value of this measurement. E.g. 3.
        Value : BigRational

        /// Unit of this measurement. E.g. Pounds.
        Unit : Unit
    }

    /// Display string.
    member this.String =
        sprintf "%A @ %A" this.Value this.Unit

module Measurement =

    /// Creates a measurement.
    let create value unit =
        {
            Value = value
            Unit = unit
        }

    /// Converts the given measurement to the given unit.
    let convert unit meas =
        if unit.BaseMap <> meas.Unit.BaseMap then
            failwithf "Can't convert '%A' to '%A'" meas.Unit unit
        let value =
            meas.Value * meas.Unit.Scale / unit.Scale
        create value unit

    /// Multiplies two measurements. E.g. 10 ft * 12 ft = 120 ft^2.
    let mult measA measB =
        create
            (measA.Value * measB.Value)
            (Unit.mult measA.Unit measB.Unit)

    /// Divides one measurement by another. E.g. 10 m / 5 s = 2 m/s.
    let div measA measB =
        create
            (measA.Value / measB.Value)
            (Unit.div measA.Unit measB.Unit)

/// https://stackoverflow.com/questions/2812084/overload-operator-in-f/2812306#2812306
/// http://nut-cracker.azurewebsites.net/blog/2011/10/05/inlinefun/

/// Measurement creation operator. E.g. 12 @ inches.
type MeasurementExt =
    | MeasurementExt

    /// Normal list concatenation operator.
    static member inline (&%) (a, _ : MeasurementExt) =
        fun b -> a @ b

    /// Creates a measurement from a rational.
    static member (&%) (value, _ : MeasurementExt) =
        fun unit -> Measurement.create value unit

    /// Creates a measurement from decimal.
    static member (&%) (value, _ : MeasurementExt) =
        BigRational.FromDecimal(value) &% MeasurementExt

    /// Creates a measurement from an integer.
    static member (&%) (value, _ : MeasurementExt) =
        BigRational.FromInt(value) &% MeasurementExt

    /// Dummy member to create ambiguity between the overloads.
    static member (&%) (_ : MeasurementExt, _ : MeasurementExt) =
        failwith "Unexpected"
        id<MeasurementExt>

[<AutoOpen>]
module MeasurementExt =

    /// Creates a measurement.
    let inline (@) a b =
        (a &% MeasurementExt) b

type Measurement with

    /// Multiples two measurements.
    static member (*)(measA, measB) =
        Measurement.mult measA measB

    /// Scales a measurement.
    static member(*)(value : int, meas) =
        Measurement.mult (value @ Unit.one) meas

    /// Scales a measurement.
    static member(*)(value : decimal, meas) =
        Measurement.mult (value @ Unit.one) meas

    /// Scales a measurement.
    static member(*)(value : BigRational, meas) =
        Measurement.mult (value @ Unit.one) meas

    /// Scales a measurement.
    static member(*)(meas, value : int) =
        Measurement.mult meas (value @ Unit.one)

    /// Scales a measurement.
    static member(*)(meas, value : decimal) =
        Measurement.mult meas (value @ Unit.one)

    /// Scales a measurement.
    static member(*)(meas, value : BigRational) =
        Measurement.mult meas (value @ Unit.one)

    /// Divides one measurement by another.
    static member (/)(measA, measB) =
        Measurement.div measA measB

    /// Scales a measurement.
    static member(/)(meas, value : int) =
        Measurement.div meas (value @ Unit.one)

    /// Scales a measurement.
    static member(/)(meas, value : decimal) =
        Measurement.div meas (value @ Unit.one)

    /// Scales a measurement.
    static member(/)(meas, value : BigRational) =
        Measurement.div meas (value @ Unit.one)

    /// Converts the given measurement to the given unit.
    static member (=>)(meas, unit) =
        Measurement.convert unit meas

    /// Creates a unit from the given measurement.
    static member (!@)(meas) =
        meas.Unit * meas.Value
