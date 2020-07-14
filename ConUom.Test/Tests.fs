namespace ConUom.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open MathNet.Numerics

open ConUom

open Standard.SI
open Standard.Imperial

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member __.Foot() =
        let unit = ft
        Assert.AreEqual(
            Set [BaseUnit.create Length "m", 1],
            unit.BaseMap |> Map.toSeq |> Set)
        Assert.AreEqual(dec 0.3048m, unit.Scale)
        Assert.AreEqual("ft", unit.Name)

    [<TestMethod>]
    member __.SquareFoot() =
        let unit = ft ^ 2
        Assert.AreEqual(
            Set [BaseUnit.create Length "m", 2],
            unit.BaseMap |> Map.toSeq |> Set)
        Assert.AreEqual(dec 0.3048m ** 2, unit.Scale)
        Assert.AreEqual("ft^2", unit.Name)

    [<TestMethod>]
    member __.MilesPerHour() =

        let unit = Unit.div mile hr
        Assert.AreEqual(
            Set [
                BaseUnit.create Length "m", 1
                BaseUnit.create Time "s", -1
            ],
            unit.BaseMap |> Map.toSeq |> Set)
        Assert.AreEqual(dec 0.44704m, unit.Scale)
        Assert.AreEqual("mile/hr", unit.Name)

        let unit = Unit.mult unit min
        Assert.AreEqual(
            Set [ BaseUnit.create Length "m", 1 ],
            unit.BaseMap |> Map.toSeq |> Set)
        Assert.AreEqual(dec 1609.344m / 60N, unit.Scale)

    [<TestMethod>]
    member __.MetersPerSecondPerSecond() =
        let unit = Unit.div m (s^2)
        Assert.AreEqual(
            Set [
                BaseUnit.create Length "m", 1
                BaseUnit.create Time "s", -2
            ],
            unit.BaseMap |> Map.toSeq |> Set)
        Assert.AreEqual(1N, unit.Scale)
        Assert.AreEqual("m/s^2", unit.Name)

    (*
    [<TestMethod>]
    member __.ConvertLength() =
        Assert.AreEqual(
            BigRational.FromDecimal(5.08m) @ cm,
            2N @ inch |> Measurement.convert centimeter)
        Assert.AreEqual(
            2N @ inch,
            BigRational.FromDecimal(5.08m) @ cm |> Measurement.convert inch)

    [<TestMethod>]
    member __.ConvertArea() =
        let ratio = BigRational.FromDecimal(2.54m)
        Assert.AreEqual(
            (6N * ratio * ratio) @ cm^2,
            (2N @ inch) * (3N @ inch) |> Measurement.convert (cm^2))
        Assert.AreEqual(
            (2N @ inch) * (3N @ inch),
            (6N * ratio * ratio) @ cm^2 |> Measurement.convert (inch^2))

    [<TestMethod>]
    member __.ConvertVolume() =
        Assert.AreEqual(
            552960N/77N @ gal,
            (10N @ ft) * (12N @ ft) * (8N @ ft) |> Measurement.convert gal)
        Assert.AreEqual(
            (10N @ ft) * (12N @ ft) * (8N @ ft),
            552960N/77N @ gal |> Measurement.convert (ft^3))

    [<TestMethod>]
    member __.Density() =
        Assert.AreEqual(
            1N @ gram,
            (1N @ cm^3) * (1N @ water))
        (*
        Assert.AreEqual(
            0N @ lb,
            (10N @ ft) * (12N @ ft) * (8N @ ft) * (1N @ water) |> Measurement.convert lb)
        *)
    *)
