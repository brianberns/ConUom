namespace ConUom

type BaseDimension =
    | Length

[<StructuredFormatDisplay("{String}")>]
type Unit =
    | BaseUnit of BaseDimension * string  // e.g. m
    | PowerUnit of (Unit * int)           // e.g. m^3
    | ProductUnit of (Unit * Unit)        // e.g. N m
    | DerivedUnit of Unit * Expr * string

    member this.String =
        match this with
            | BaseUnit (_, name) -> name
            | PowerUnit (unit, n) -> sprintf "%A^%d" unit n
            | ProductUnit (unitA, unitB) -> sprintf "%A %A" unitA unitB
            | DerivedUnit (_, _, name) -> name

[<AutoOpen>]
module Unit =

    let (^) unit = function
        | 0 -> failwith "Invalid argument"
        | 1 -> unit
        | n -> PowerUnit (unit, n)

    let centi = Quotient (Var, Const 100m)

    let meter = BaseUnit (Length, "m")
    let m = meter

    let centimeter = DerivedUnit (meter, centi, "cm")
    let cm = centimeter

    let inch = DerivedUnit (centimeter, Product (Var, Const 2.54m), "in")

    let gallon = DerivedUnit (inch ^ 3, Product (Var, Const 231m), "gal")
