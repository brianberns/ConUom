namespace ConUom

type BaseDimension =
    | Length
    | Mass

[<StructuredFormatDisplay("{String}")>]
type Unit =
    | BaseUnit of BaseDimension * string    // e.g. m
    | PowerUnit of (Unit * int)             // e.g. ft^3
    | ProductUnit of (Unit * Unit)          // e.g. N m
    | QuotientUnit of (Unit * Unit)         // e.g. gram/(cm^3)
    | DerivedUnit of Unit * Expr * string   // expr converts from outer unit to inner unit

    member this.String =
        match this with
            | BaseUnit (_, name) -> name
            | PowerUnit (unit, n) -> sprintf "%A^%d" unit n
            | ProductUnit (unitA, unitB) -> sprintf "%A %A" unitA unitB
            | QuotientUnit (unitA, unitB) -> sprintf "%A/%A" unitA unitB
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
            match unitA, unitB with
                | _, PowerUnit (unit, power) ->
                    if unitA = unit then
                        PowerUnit (unit, power + 1)
                    else
                        ProductUnit (unitA, unitB)
                | PowerUnit (unit, power), _ ->
                    if unitB = unit then
                        PowerUnit (unit, power + 1)
                    else
                        ProductUnit (unitA, unitB)
                | _ -> ProductUnit (unitA, unitB)

[<AutoOpen>]
module UnitAutoOpen =
    let (^) = Unit.power
