namespace ConUom.Test

open System
open System.Linq
open System.Net

open Microsoft.VisualStudio.TestTools.UnitTesting
open MathNet.Numerics
open FsCheck
open ConUom

/// Sample calculations:
/// https://frinklang.org/#SampleCalculations
[<TestClass>]
type SampleCalculations () =

    let centi = 1N/ 100N
    let milli = 1N/1000N

    let one = Unit.One
    let percent = centi * one

    let m = Unit("m")
    let cm = centi * m

    let kg = Unit("kg")
    let g = milli * kg

    let cc = 1N * (cm^3)
    let liter = 1000N * cc

    let inch = 2.54m * cm
    let foot = 12N * inch
    let ft = foot

    let pound = 0.45359237m * kg 
    let lb = pound
    let ton = 2000 * lb

    let gal = 231 * (inch^3)
    let qt = 0.25m * gal
    let pint = 0.5m * qt
    let floz = (1N/16N) * pint
    let water = 1 @ g/cc
    let alcohol = 0.7893m @ g/cc

    let assertEq(measA : Measurement, measB : Measurement) =
        Assert.AreEqual(measA.Value, measB.Value)
        Assert.IsTrue(measA.Unit.BaseUnits.SequenceEqual(measB.Unit.BaseUnits))
        Assert.AreEqual(measA.Unit.Scale, measB.Unit.Scale)

    let mfloat (meas : Measurement) = meas.Value |> float

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
        assertEq(
            5.08m @ cm,
            (2 @ inch) => cm)
        assertEq(
            2 @ inch,
            (5.08m @ cm) => inch)

    [<TestMethod>]
    member __.Area() =
        let ratio = BigRational.FromDecimal(2.54m)
        assertEq(
            6N * ratio * ratio @ cm^2,
            (2 @ inch) * (3 @ inch) => cm^2)
        assertEq(
            (2 @ inch) * (3 @ inch),
            (6N * ratio * ratio @ cm^2) => inch^2)

    [<TestMethod>]
    member __.Volume() =
        assertEq(
            552960N/77N @ gal,
            (10 @ ft) * (12 @ ft) * (8 @ ft) => gal)
        assertEq(
            (10 @ ft) * (12 @ ft) * (8 @ ft),
            (552960N/77N @ gal) => ft^3)

    [<TestMethod>]
    member __.Density() =
        assertEq(
            1 @ g,
            (1 @ cm^3) * water)
        Assert.AreEqual(
            59930.84,
            (10 @ ft) * (12 @ ft) * (8 @ ft) * water => lb |> mfloat,
            0.01)
        Assert.AreEqual(
            0.5339,
            (2 @ ton) / ((10 @ ft) * (12 @ ft) * water) => ft |> mfloat,
            0.0001)

    [<TestMethod>]
    member __.Liquor() =

        let beer = (12 @ floz) * (3.2m @ percent) * (water/alcohol)
        let magnum = 1.5m @ liter
        Assert.AreEqual(
            14.07,
            magnum * (13.5m @ percent) => !@beer |> mfloat,
            0.01)

        let proof = 1N/200N * one
        let junglejuice = (1.75m @ liter) * (190 @ proof) / (5 @ gal)
        Assert.AreEqual(
            8.78,
            junglejuice => percent |> mfloat,
            0.01)

        Assert.AreEqual(
            10.83,
            5 * (12 @ floz) * junglejuice => !@beer |> mfloat,
            0.01)

[<TestClass>]
type Parser () =

    let assertEq(itemsA : seq<'t>, itemsB : seq<'t>) =
        itemsA.SequenceEqual(itemsB)

    [<TestMethod>]
    member __.ParsePlanckMass() =
        let lookup, msgOpt = Frink.parse "length    =!= m // meter
time      =!= s // second
mass      =!=  kg   // kilogram
N :=              kg m / s^2  // force
pi :=                  3.141592653589793238
hbar :=                (6.62607015ee-34 N m s) / (2 pi)
c :=                   299792458 m/s   // speed of light
G :=             6.67430e-11 N m^2 / kg^2  // gravity
planck_mass :=          (hbar c / G)^(1/2)"
        msgOpt |> Option.iter Assert.Fail
        let unit = lookup.Units.["planck_mass"]
        Assert.AreEqual(2.1764e-8, float unit.Scale, 0.0001e-8)
        assertEq(
            [ "kg", 1],
            unit.BaseUnits)

    [<TestMethod>]
    member __.ParsePower() =
        let lookup, msgOpt = Frink.parse "
length =!= m
time =!= s
mass =!= kg
m^2  kg s^-3 ||| power"
        msgOpt |> Option.iter Assert.Fail
        let unit = lookup.Units.["power"]
        Assert.AreEqual(1N, unit.Scale)
        assertEq(
            [
                "m", 2
                "kg", 1
                "s", -3
            ],
            unit.BaseUnits)

    [<TestMethod>]
    member __.ParseSurvey() =
        let unitMap, msgOpt = Frink.parse "
length =!= m
inch := 2.54ee-2 m
ft := 12 inch
survey ::- 1200/3937 m/ft  // survey length ratio"
        msgOpt |> Option.iter Assert.Fail
        let scale = unitMap.Prefixes.["survey"]
        Assert.AreEqual((1200N/3937N) / (3048N/10000N), scale)

    [<TestMethod>]
    member __.ParseHubbleConstant() =
        let lookup, msgOpt = Frink.parse "
mega ::- 1ee6
length =!= m
km = 1000 m
time =!= s
au := 149597870700 m
arcsec := 1/60 1/60 1/360 2 3.141592653589793238
parsec := au / arcsec
hubble_constant := 67.8 km/s/megaparsec
age_of_universe = 1/hubble_constant"
        msgOpt |> Option.iter Assert.Fail
        let unit = lookup.Units.["age_of_universe"]
        Assert.AreEqual(4.5511e17, float unit.Scale, 0.0001e17)
        assertEq(
            [ "s", 1 ],
            unit.BaseUnits)

    [<TestMethod>]
    member __.ParseLightyear() =
        let lookup, msgOpt = Frink.parse "
length =!= m
time =!= s
c := 299792458 m/s
min := 60 s
hr := 60 min
day := 24 hr
lightyear := c (365 + 1/4) day"
        msgOpt |> Option.iter Assert.Fail
        let unit = lookup.Units.["lightyear"]
        Assert.AreEqual(9460730472580800N, unit.Scale)
        assertEq(
            [ "m", 1 ],
            unit.BaseUnits)

    [<TestMethod>]
    member __.ParseUrl() =

        use client = new WebClient()
        let lookup, msgOpt =
            client.DownloadString("https://frinklang.org/frinkdata/units.txt")
                |> Frink.parse
        msgOpt |> Option.iter Assert.Fail

        let km = lookup?km
        let megaparsec = lookup?megaparsec
        let s = lookup?s
        let gigayear = lookup?gigayear   // billions of years

        let hubble = 73 @ km/s/megaparsec
        let universe = 1/hubble => gigayear
        Assert.AreEqual(13.39, float universe.Value, 0.01)

type MyGenerators () =

    static member BigRational () =

        let genRational =
            gen {
                let! n = Arb.generate<int>
                let! d = Arb.generate<NonZeroInt>
                return BigRational.FromIntFraction(n, d.Get)
            }

        {
            new Arbitrary<BigRational>() with
                override __.Generator = genRational
        }

    static member Unit() =

        let genBaseUnit =
            gen {
                let! name =
                    Gen.choose(0, 25)
                        |> Gen.map (fun i ->
                            int 'a' + i
                                |> char
                                |> string)
                return Unit(name)
            }

        let genDimensionlessRationalUnit =
            gen {
                let! scale = Arb.generate<BigRational>
                return Unit(scale)
            }

        let genDimensionlessDecimalUnit =
            gen {
                let! scale = Arb.generate<decimal>
                return Unit(scale)
            }

        let genDimensionlessIntUnit =
            gen {
                let! scale = Arb.generate<int>
                return Unit(scale)
            }

        let genDimensionlessUnit =
            Gen.oneof [
                genDimensionlessRationalUnit
                genDimensionlessDecimalUnit
                genDimensionlessIntUnit
            ]

        let genDerivedUnit units =
            gen {
                let len = units |> Array.length
                let! i = Gen.choose(0, len - 1)
                let unit = units.[i]
                let! scale = genDimensionlessUnit
                return scale * unit
            }

        let genUnit units =
            Gen.frequency [
                1, genBaseUnit
                1, genDimensionlessUnit
                // 1, genDerivedUnit units
            ]

        {
            new Arbitrary<Unit>() with
                override __.Generator = genUnit Array.empty
        }

[<TestClass>]
type FsCheck () =

    let equalUnits (unitA : Unit) (unitB : Unit) =
        unitA.Scale = unitB.Scale
            && unitA.BaseUnits.SequenceEqual(unitB.BaseUnits)

    let equalMeasurements (measA : Measurement) (measB : Measurement) =
        measA.Value = measB.Value
            && equalUnits measA.Unit measB.Unit

    let isConvertible (meas : Measurement) unit =
        let converted = meas => unit
        let convertedBack = converted => meas.Unit
        equalMeasurements convertedBack meas

    let check (meas : Measurement) (unit : Unit) =
        let condition =
            meas.Unit.BaseUnits.SequenceEqual(unit.BaseUnits)
                && meas.Unit.Scale <> 0N
                && unit.Scale <> 0N
        condition ==> lazy isConvertible meas unit

    [<ClassInitialize>]
    static member Init(context : TestContext) =
        Arb.register<MyGenerators>() |> ignore

    [<TestMethod>]
    member __.Test() =
        Check.QuickThrowOnFailure(check)
