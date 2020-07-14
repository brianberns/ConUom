namespace ConUom

open MathNet.Numerics

/// Extended product operator.
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
        fun measB -> Measurement.mult measA measB

    /// Dummy member to create ambiguity between the overloads.
    static member (=>) (_ : ProductExt, _ : ProductExt) =
        failwith "Unexpected"
        id<ProductExt>

[<AutoOpen>]
module ProductExt =

    /// Extended product operator.
    let inline (*) a b =
        (a => ProductExt) b

/// Extended quotient operator.
type QuotientExt =
    | QuotientExt

    /// Normal arithmetic quotient.
    static member inline (=>) (a, _ : QuotientExt) =
        fun b -> a / b

    /// Unit quotient.
    static member (=>) (unitA, _ : QuotientExt) =
        fun unitB -> Unit.div unitA unitB

    /// Measurement quotient.
    static member (=>) (measA, _ : QuotientExt) =
        fun measB -> Measurement.div measA measB

    /// Dummy member to create ambiguity between the overloads.
    static member (=>) (_ : QuotientExt, _ : QuotientExt) =
        failwith "Unexpected"
        id<QuotientExt>

[<AutoOpen>]
module QuotientExt =

    /// Extended quotient operator.
    let inline (/) a b =
        (a => QuotientExt) b

/// Extended @ operator.
type AtExt =
    | AtExt

    /// Create measurement from rational.
    static member (=>) (value, _ : AtExt) =
        fun unit -> Measurement.create value unit

    /// Create measurement from decimal.
    static member (=>) (value, _ : AtExt) =
        fun unit -> Measurement.create (dec value) unit

    /// Create measurement from integer.
    static member (=>) (value, _ : AtExt) =
        fun unit -> Measurement.create (BigRational.FromInt value) unit

    /// Dummy member to create ambiguity between the overloads.
    static member (=>) (_ : AtExt, _ : AtExt) =
        failwith "Unexpected"
        id<AtExt>

[<AutoOpen>]
module AtExt =

    /// Extended @ operator.
    let inline (@) a b =
        (a => AtExt) b
