namespace ConUom.Test

open System

open Microsoft.VisualStudio.TestTools.UnitTesting

open MathNet.Numerics

open ConUom

open Standard
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
    member __.Length() =
        Assert.AreEqual(
            BigRational.FromDecimal(5.08m) @ cm,
            2N @ inch |> Measurement.convert centimeter)
        Assert.AreEqual(
            2N @ inch,
            BigRational.FromDecimal(5.08m) @ cm |> Measurement.convert inch)

    [<TestMethod>]
    member __.Area() =
        let ratio = BigRational.FromDecimal(2.54m)
        Assert.AreEqual(
            (6N * ratio * ratio) @ cm^2,
            (2N @ inch) * (3N @ inch) |> Measurement.convert (cm^2))
        Assert.AreEqual(
            (2N @ inch) * (3N @ inch),
            (6N * ratio * ratio) @ cm^2 |> Measurement.convert (inch^2))

    [<TestMethod>]
    member __.Volume() =
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

    [<TestMethod>]
    member __.Liquor() =
        Assert.AreEqual(
            Unit.createEmpty (10000N/7893N),
            water/alcohol)
        let beer = (12N @@ floz) * (dec 3.2m @@ percent) * (water/alcohol)
        Assert.AreEqual(
            (2219906250N/157725491N) @ beer,
            (1N @ magnum) * (dec 13.5m @ percent) |> Measurement.convert beer)
