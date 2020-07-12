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

module Unit =

    let power unit = function
        | 0 -> failwith "Invalid argument"
        | 1 -> unit
        | n -> PowerUnit (unit, n)

    let product unitA unitB =
        if unitA = unitB then
            PowerUnit (unitA, 2)
        else
            ProductUnit (unitA, unitB)

[<AutoOpen>]
module UnitAutoOpen =
    let (^) = Unit.power
