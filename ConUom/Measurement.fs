namespace ConUom

open System.Runtime.CompilerServices
open MathNet.Numerics

/// An individual measurement. E.g. 3 lbs.
[<StructuredFormatDisplay("{String}")>]
type Measurement(value, unit) =

    /// Value of this measurement. E.g. 3.
    member _.Value : BigRational =
        value

    /// Unit of this measurement. E.g. Pounds.
    member _.Unit : Unit =
        unit

    /// Display string.
    member private this.String =
        sprintf "%A @ %A" this.Value this.Unit

    /// Display string.
    override meas.ToString() =
        meas.String

    /// Converts this measurement to the given unit.
    member meas.ConvertTo(unit : Unit) =
        if unit.BaseMap <> meas.Unit.BaseMap then
            failwithf "Can't convert '%A' to '%A'" meas.Unit unit
        if unit.Scale = 0N then
            failwithf "Can't convert to zero unit '%A'" unit
        let value =
            meas.Value * meas.Unit.Scale / unit.Scale
        Measurement(value, unit)

    /// Converts the given measurement to the given unit.
    static member (=>)(meas : Measurement, unit) =
        meas.ConvertTo(unit)

    /// Negates a measurement.
    static member (~-)(meas : Measurement) =
        Measurement(-meas.Value, meas.Unit)

    /// Adds two measurements.
    static member (+)(measA : Measurement, measB : Measurement) =
        let measB' = measB => measA.Unit
        Measurement(
            measA.Value + measB'.Value,
            measA.Unit)

    /// Subtracts one measurement from another.
    static member (-)(measA : Measurement, measB : Measurement) =
        measA + (-measB)

    /// Multiplies two measurements. E.g. 10 ft * 12 ft = 120 ft^2.
    static member (*)(measA : Measurement, measB : Measurement) =
        Measurement(
            measA.Value * measB.Value,
            measA.Unit * measB.Unit)

    /// Scales a measurement.
    static member(*)(value : BigRational, meas : Measurement) =
        Measurement(
            value * meas.Value,
            meas.Unit)

    /// Scales a measurement.
    static member(*)(value, meas : Measurement) =
        (value |> BigRational.FromInt) * meas

    /// Scales a measurement.
    static member(*)(value, meas : Measurement) =
        (value |> BigRational.FromDecimal) * meas

    /// Scales a measurement.
    static member(*)(meas : Measurement, value : BigRational) =
        value * meas

    /// Scales a measurement.
    static member(*)(meas : Measurement, value) =
        meas * (value |> BigRational.FromInt)

    /// Scales a measurement.
    static member(*)(meas : Measurement, value) =
        meas * (value |> BigRational.FromDecimal)

    /// Divides one measurement by another. E.g. 10 m / 5 s = 2 m/s.
    static member (/)(measA : Measurement, measB : Measurement) =
        Measurement(
            measA.Value / measB.Value,
            measA.Unit / measB.Unit)

    /// Scales a measurement.
    static member(/)(meas : Measurement, value : BigRational) =
        meas / Measurement(value, Unit.One)

    /// Scales a measurement.
    static member(/)(meas : Measurement, value) =
        meas / (value |> BigRational.FromInt)

    /// Scales a measurement.
    static member(/)(meas : Measurement, value) =
        meas / (value |> BigRational.FromDecimal)

    /// Inverts a measurement.
    static member(/)(value : BigRational, meas : Measurement) =
        Measurement(value, Unit.One) / meas

    /// Inverts a measurement.
    static member(/)(value, meas : Measurement) =
        (value |> BigRational.FromInt) / meas

    /// Inverts a measurement.
    static member(/)(value, meas : Measurement) =
        (value |> BigRational.FromDecimal) / meas

    /// Creates a unit from the given measurement.
    static member (!@)(meas : Measurement) =
        meas.Unit * meas.Value

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
        fun unit -> Measurement(value, unit)

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

/// Extensions for use from C#.
[<Extension>]
type MeasurementCsExt =

    /// Creates a measurement.
    [<Extension>]
    static member Measure(unit, value) =
        Measurement(value, unit)

    /// Creates a measurement.
    [<Extension>]
    static member Measure(unit, value) =
        Measurement(value |> BigRational.FromInt, unit)

    /// Creates a measurement.
    [<Extension>]
    static member Measure(unit, value) =
        Measurement(value |> BigRational.FromDecimal, unit)
