namespace ConUom.Test

open System

open Microsoft.VisualStudio.TestTools.UnitTesting

open MathNet.Numerics

open ConUom
open ConUom.Units
open ConUom.Units.SI.Density
open ConUom.Units.SI.Length
open ConUom.Units.SI.Mass
open ConUom.Units.SI.Volume
open ConUom.Units.USCS.Length
open ConUom.Units.USCS.Mass
open ConUom.Units.USCS.Volume

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
            Assert.AreEqual(rational, BigRational.FromDecimal decimal)

    [<TestMethod>]
    member __.Length() =
        Assert.AreEqual(
            BigRational.FromDecimal(5.08m) @ cm,
            2 @ inch |> Measurement.convert centimeter)
        Assert.AreEqual(
            2 @ inch,
            BigRational.FromDecimal(5.08m) @ cm |> Measurement.convert inch)

    [<TestMethod>]
    member __.Area() =
        let ratio = BigRational.FromDecimal(2.54m)
        Assert.AreEqual(
            (6N * ratio * ratio) @ cm^2,
            (2 @ inch) * (3 @ inch) |> Measurement.convert (cm^2))
        Assert.AreEqual(
            (2 @ inch) * (3 @ inch),
            (6N * ratio * ratio) @ cm^2 |> Measurement.convert (inch^2))

    [<TestMethod>]
    member __.Volume() =
        Assert.AreEqual(
            552960N/77N @ gal,
            (10 @ ft) * (12 @ ft) * (8 @ ft) |> Measurement.convert gal)
        Assert.AreEqual(
            (10 @ ft) * (12 @ ft) * (8 @ ft),
            552960N/77N @ gal |> Measurement.convert (ft^3))

    [<TestMethod>]
    member __.Density() =
        Assert.AreEqual(
            1 @ gram,
            (1 @ cm^3) * (1 @ water))
        Assert.AreEqual(
            2718417272832N/45359237N @ lb,
            (10 @ ft) * (12 @ ft) * (8 @ ft) * (1 @ water) |> Measurement.convert lb)

    [<TestMethod>]
    member __.Liquor() =
        let magnum = 1.5m @@ liter
        let alcohol = 0.7893m @@ density
        Assert.AreEqual(
            Unit.createScale (10000N/7893N),
            water/alcohol)
        let beer = (12 @@ floz) * (3.2m @@ percent) * (water/alcohol)
        Assert.AreEqual(
            (2219906250N/157725491N) @ beer,
            (1 @ magnum) * (13.5m @ percent) |> Measurement.convert beer)
