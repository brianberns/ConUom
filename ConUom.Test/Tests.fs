namespace ConUom.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open ConUom

open Standard.SI
open Standard.Imperial

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member __.InchesToMeters() =
        Assert.AreEqual(
            0.0508 @ m,
            2 @ inch |> Measure.simplify)

    [<TestMethod>]
    member __.SquareFeet() =
        Assert.AreEqual(
            120 @ ft^2,
            (10 @ ft) * (12 @ ft))
