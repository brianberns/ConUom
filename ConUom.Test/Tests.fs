namespace ConUom.Test

open System

open Microsoft.VisualStudio.TestTools.UnitTesting

open MathNet.Numerics

open ConUom
open ConUom.Measurements.Density
open ConUom.Units
open ConUom.Units.SI.Length
open ConUom.Units.SI.Mass
open ConUom.Units.SI.Volume
open ConUom.Units.USCS.Length
open ConUom.Units.USCS.Mass
open ConUom.Units.USCS.Volume

[<TestClass>]
type TestClass () =

    let float = Measurement.float

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
            5.08m @ cm,
            (2 @ inch) => centimeter)
        Assert.AreEqual(
            2 @ inch,
            (5.08m @ cm) => inch)

    [<TestMethod>]
    member __.Area() =
        let ratio = BigRational.FromDecimal(2.54m)
        Assert.AreEqual(
            6N * ratio * ratio @ cm^2,
            (2 @ inch) * (3 @ inch) => cm^2)
        Assert.AreEqual(
            (2 @ inch) * (3 @ inch),
            (6N * ratio * ratio @ cm^2) => inch^2)

    [<TestMethod>]
    member __.Volume() =
        Assert.AreEqual(
            552960N/77N @ gal,
            (10 @ ft) * (12 @ ft) * (8 @ ft) => gal)
        Assert.AreEqual(
            (10 @ ft) * (12 @ ft) * (8 @ ft),
            (552960N/77N @ gal) => ft^3)

    [<TestMethod>]
    member __.Density() =
        Assert.AreEqual(
            1 @ gram,
            (1 @ cm^3) * water)
        Assert.AreEqual(
            59930.84215309883,
            (10 @ ft) * (12 @ ft) * (8 @ ft) * water => lb |> float)
        Assert.AreEqual(
            0.5339487791320047,
            (2 @ ton) / ((10 @ ft) * (12 @ ft) * water) => ft |> float)

    [<TestMethod>]
    member __.Liquor() =

        let beer = (12 @ floz) * (3.2m @ percent) * (water/alcohol)
        let magnum = 1.5m @ liter
        Assert.AreEqual(
            14.074492562524341,
            magnum * (13.5m @ percent) => !@beer |> float)

        let proof = 1N/200N @@ one
        let junglejuice = (1.75m @ liter) * (190 @ proof) / (5 @ gallon)
        Assert.AreEqual(
            8.78372074090843481138500000,
            junglejuice => percent |> float)

        Assert.AreEqual(
            10.83279809499848,
            (5 @ one) * (12 @ floz) * junglejuice => !@beer |> float)
