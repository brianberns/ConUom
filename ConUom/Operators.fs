namespace ConUom

/// https://stackoverflow.com/questions/2812084/overload-operator-in-f/2812306#2812306
/// http://nut-cracker.azurewebsites.net/blog/2011/10/05/inlinefun/

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
