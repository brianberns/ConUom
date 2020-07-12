namespace ConUom.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open ConUom

open Standard.SI
open Standard.Imperial

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member __.Invert() =
        let cToF = Sum (Product (Var, Const 1.8m), Const 32m)
        let fToC = Quotient ((Difference (Var, Const 32m)), Const 1.8m)
        Assert.AreEqual(fToC, Expr.invert cToF)
        Assert.AreEqual(cToF, Expr.invert fToC)

    [<TestMethod>]
    member __.ConvertLength() =
        Assert.AreEqual(
            5.08 @ cm,
            2 @ inch |> Measure.convert centimeter)
        Assert.AreEqual(
            2 @ inch,
            5.08 @ cm |> Measure.convert inch)

    [<TestMethod>]
    member __.ConvertArea() =
        Assert.AreEqual(
            (6m * 2.54m * 2.54m) @ cm^2,
            (2 @ inch) * (3 @ inch) |> Measure.convert (cm^2))

    [<TestMethod>]
    member __.ConvertVolume() =
        Assert.AreEqual(
            552960m/77m @ gal,
            (10 @ ft) * (12 @ ft) * (8 @ ft) |> Measure.convert gal)
