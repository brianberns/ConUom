namespace ConUom.Test

open System

open Microsoft.VisualStudio.TestTools.UnitTesting

open MathNet.Numerics

open ConUom

open Standard.SI
open Standard.Imperial

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member __.FromDecimal() =
        let pairs =
            [
                1M, 1N
                100000000000000M, 100000000000000N
                10000000000000000000000000000M, 10000000000000000000000000000N
                100000000000000.00000000000000M, 100000000000000N
                1.0000000000000000000000000000M, 1N
                123456789M, 123456789N
                0.123456789M, 123456789N/1000000000N
                0.000000000123456789M, 123456789N/1000000000000000000N
                0.000000000000000000123456789M, 123456789N/1000000000000000000000000000N
                4294967295M, 4294967295N
                18446744073709551615M, 18446744073709551615N
                Decimal.MaxValue, 79228162514264337593543950335N
                Decimal.MinValue, -79228162514264337593543950335N
                -7.9228162514264337593543950335M, -79228162514264337593543950335N/10000000000000000000000000000N
            ]
        for (decimal, rational) in pairs do
            Assert.AreEqual(rational, dec decimal)

    [<TestMethod>]
    member __.Foot() =
        let unit = ft
        Assert.AreEqual(
            Set [BaseUnit.create Length "m", 1],
            unit.BaseUnits)
        Assert.AreEqual(dec 0.3048m, unit.Scale)

    [<TestMethod>]
    member __.SquareFoot() =
        let unit = ft ^ 2
        Assert.AreEqual(
            Set [BaseUnit.create Length "m", 2],
            unit.BaseUnits)
        Assert.AreEqual(dec 0.3048m ** 2, unit.Scale)

    [<TestMethod>]
    member __.MilesPerHour() =

        let unit = Unit.div mile hr
        Assert.AreEqual(
            Set [
                BaseUnit.create Length "m", 1
                BaseUnit.create Time "s", -1
            ],
            unit.BaseUnits)
        Assert.AreEqual(dec 0.44704m, unit.Scale)

        let unit = unit * min
        Assert.AreEqual(
            Set [ BaseUnit.create Length "m", 1 ],
            unit.BaseUnits)
        Assert.AreEqual(dec 1609.344m / 60N, unit.Scale)

    [<TestMethod>]
    member __.MetersPerSecondPerSecond() =
        let unit = Unit.div (Unit.div m s) s
        Assert.AreEqual(
            Set [
                BaseUnit.create Length "m", 1
                BaseUnit.create Time "s", -2
            ],
            unit.BaseUnits)
        Assert.AreEqual(1N, unit.Scale)

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
        Assert.AreEqual(
            2718417272832N/45359237N @ lb,
            (10N @ ft) * (12N @ ft) * (8N @ ft) * (1N @ water) |> Measurement.convert lb)
