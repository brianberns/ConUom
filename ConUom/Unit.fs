namespace ConUom

open MathNet.Numerics

type BaseDimension =
    | Dimensionless
    | Length
    | Mass

[<StructuredFormatDisplay("{String}")>]
type Unit =
    | BaseUnit of BaseDimension * string    // e.g. m
    | PowerUnit of (Unit * int)             // e.g. ft^3
    | ProductUnit of (Unit * Unit)          // e.g. gram cm^-3 = gram/cm^3
    | DerivedUnit of Unit * Expr * string   // expr converts from outer unit to inner unit

    member this.String =
        match this with
            | BaseUnit (_, name) -> name
            | PowerUnit (unit, n) -> sprintf "%A^%d" unit n
            | ProductUnit (unitA, unitB) -> sprintf "%A %A" unitA unitB
            | DerivedUnit (_, _, name) -> name

module Unit =

    let one = BaseUnit (Dimensionless, "1")

    /// An integer power of a rational base.
    let rec private exp x y =
        match y with
            | 0 -> 1N
            | 1 -> x
            | _ ->
                if y > 1 then
                    x * (exp x (y - 1))
                else
                    failwith "Unexpected"

    /// Creates an expression that simplifies the given unit.
    let simplify unit =

        let rec loop outer = function

            | DerivedUnit (unit, inner, _) ->
                loop (Expr.subst outer inner) unit

                // E.g. ft^2 = 2.54^2 cm^2
            | PowerUnit (DerivedUnit (unit, inner, _), power) ->
                let expr =
                    match inner with
                        | Product (Const x, Var)
                        | Product (Var, Const x) ->
                            Product (Const (exp x power), Var)
                        | Quotient (Var, Const x) ->
                            Quotient (Var, Const (exp x power))
                        | _ -> failwith "Unexpected"
                        |> Expr.subst outer
                let unit' =
                    PowerUnit (unit, power)
                loop expr unit'

            | unit -> outer, unit

        loop Var unit

    let power unit = function
        | 0 -> failwith "Invalid argument"
        | 1 -> unit
        | n -> PowerUnit (unit, n)

    let product unitA unitB =
        if unitA = unitB then
            PowerUnit (unitA, 2)
        else
            match unitA, unitB with
                | PowerUnit (unitA', a), PowerUnit (unitB', b)
                    when unitA' = unitB' ->
                    if a = b then one
                    else PowerUnit (unitA', a - b)
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
