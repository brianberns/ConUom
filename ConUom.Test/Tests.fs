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

    [<TestMethod>]
    member __.Invert() =
        let cToF = Sum (Product (Var, Const 1.8m), Const 32m)
        let fToC = Quotient ((Difference (Var, Const 32m)), Const 1.8m)
        Assert.AreEqual(fToC, Expr.invert cToF)
        Assert.AreEqual(cToF, Expr.invert fToC)
