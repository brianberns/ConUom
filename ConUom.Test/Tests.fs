namespace ConUom.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open ConUom

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member __.Test() =
        Assert.AreEqual(
            0.0508 @ m,
            2 @ inch |> Measure.simplify)
