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

    /// Answers the given measurement's value as a floating point number.
    let float meas =
        float meas.Value

    /// Converts the given measurement to the given unit.
    let convert unit meas =
        let baseUnits = unit |> Unit.baseUnits
        if baseUnits <> (meas.Unit |> Unit.baseUnits) then
            failwithf "Can't convert '%s' to '%s'" meas.Unit.Name unit.Name
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

type Measurement with

    /// Multiples two measurements.
    static member (*)(measA, measB) =
        Measurement.mult measA measB

    /// Divides one measurement by another.
    static member (/)(measA, measB) =
        Measurement.div measA measB

    /// Converts the given measurement to the given unit.
    static member (=>)(meas, unit) =
        Measurement.convert unit meas

    /// Creates a unit from the given measurement.
    static member (!@)(meas) =
        meas.Unit * meas.Value

/// https://stackoverflow.com/questions/2812084/overload-operator-in-f/2812306#2812306
/// http://nut-cracker.azurewebsites.net/blog/2011/10/05/inlinefun/

/// Measurement creation operator.
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
