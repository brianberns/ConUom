namespace ConUom.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open MathNet.Numerics

open ConUom

open Standard.SI
open Standard.Imperial

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member __.Invert() =
        let cToF = Sum (Product (Var, Const (18N/10N)), Const 32N)
        let fToC = Quotient ((Difference (Var, Const 32N)), Const (18N/10N))
        Assert.AreEqual(fToC, Expr.invert cToF)
        Assert.AreEqual(cToF, Expr.invert fToC)

    [<TestMethod>]
    member __.ConvertLength() =
        Assert.AreEqual(
            508N/100N @ cm,
            2N @ inch |> Measure.convert centimeter)
        Assert.AreEqual(
            2N @ inch,
            508N/100N @ cm |> Measure.convert inch)

    [<TestMethod>]
    member __.ConvertArea() =
        Assert.AreEqual(
            (6N * 254N/100N * 254N/100N) @ cm^2,
            (2N @ inch) * (3N @ inch) |> Measure.convert (cm^2))

    [<TestMethod>]
    member __.ConvertVolume() =
        Assert.AreEqual(
            552960N/77N @ gal,
            (10N @ ft) * (12N @ ft) * (8N @ ft) |> Measure.convert gal)
