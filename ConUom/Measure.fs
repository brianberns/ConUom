namespace ConUom

[<StructuredFormatDisplay("{String}")>]
type Measure =
    {
        Value : decimal
        Unit : Unit
    }

    member this.String =
        sprintf "%M %A" this.Value this.Unit

[<AutoOpen>]
module Measure =

    let inline (@) value unit =
        {
            Value = decimal value
            Unit = unit
        }

    let rec simplify measure =
        match measure.Unit with
            | DerivedUnit (unit, convert, _) ->
                simplify {
                    Value = convert |> Expr.eval measure.Value
                    Unit = unit
                }
            | _ -> measure
