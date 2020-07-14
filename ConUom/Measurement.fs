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
        let baseUnits = (unit : Unit).BaseUnits
        if baseUnits <> meas.Unit.BaseUnits then
            failwithf "Can't convert '%s' to '%s'" meas.Unit.Name unit.Name
        let value =
            meas.Value * meas.Unit.Scale / unit.Scale
        create value unit

    /// Multiplies two measurements. E.g. 10 ft * 12 ft = 120 ft^2.
    let mult measA measB =
        create
            (measA.Value * measB.Value)
            (Unit.mult measA.Unit measB.Unit)

    /// Divides two measurements. E.g. 10 m / 5 s = 2 m/s.
    let div measA measB =
        create
            (measA.Value / measB.Value)
            (Unit.div measA.Unit measB.Unit)

/// Measurement creation operator.
type MeasurementExt =
    | MeasurementExt

    /// Create measurement from rational.
    static member (&%) (value, _ : MeasurementExt) =
        fun unit -> Measurement.create value unit

    /// Create measurement from decimal.
    static member (&%) (value, _ : MeasurementExt) =
        fun unit -> Measurement.create (BigRational.FromDecimal value) unit

    /// Create measurement from integer.
    static member (&%) (value, _ : MeasurementExt) =
        fun unit -> Measurement.create (BigRational.FromInt value) unit

    /// Dummy member to create ambiguity between the overloads.
    static member (&%) (_ : MeasurementExt, _ : MeasurementExt) =
        failwith "Unexpected"
        id<MeasurementExt>

[<AutoOpen>]
module MeasurementExt =

    /// Creates a measurement.
    let inline (@) a b =
        (a &% MeasurementExt) b

    /// Converts the given measurement to the given unit.
    let (=>) meas unit =
        Measurement.convert unit meas
