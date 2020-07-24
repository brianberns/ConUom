namespace ConUom.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open MathNet.Numerics
open ConUom

[<TestClass>]
type TestClass () =

    let centi = 1N/ 100N
    let milli = 1N/1000N

    let one = Unit.one
    let percent = centi * one

    let m = Unit.createBase "Length" "m"
    let cm = centi * m

    let kg = Unit.createBase "Mass" "kg"
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

    let mfloat = Measurement.float

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
            (2 @ inch) => cm)
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
            1 @ g,
            (1 @ cm^3) * water)
        Assert.AreEqual(
            59930.84215309883,
            (10 @ ft) * (12 @ ft) * (8 @ ft) * water => lb |> mfloat)
        Assert.AreEqual(
            0.5339487791320047,
            (2 @ ton) / ((10 @ ft) * (12 @ ft) * water) => ft |> mfloat)

    [<TestMethod>]
    member __.Liquor() =

        let beer = (12 @ floz) * (3.2m @ percent) * (water/alcohol)
        let magnum = 1.5m @ liter
        Assert.AreEqual(
            14.074492562524341,
            magnum * (13.5m @ percent) => !@beer |> mfloat)

        let proof = 1N/200N * one
        let junglejuice = (1.75m @ liter) * (190 @ proof) / (5 @ gal)
        Assert.AreEqual(
            8.78372074090843481138500000,
            junglejuice => percent |> mfloat)

        Assert.AreEqual(
            10.83279809499848,
            (5 @ one) * (12 @ floz) * junglejuice => !@beer |> mfloat)

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
        Assert.AreEqual(2.1764343427179E-08, float unit.Scale)
        Assert.AreEqual(
            set [ BaseUnit.create "mass" "kg", 1],
            unit |> Unit.baseUnits)

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
        Assert.AreEqual(
            set [
                BaseUnit.create "length" "m", 2
                BaseUnit.create "mass" "kg", 1
                BaseUnit.create "time" "s", -3
            ],
            unit |> Unit.baseUnits)

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
        Assert.AreEqual(4.551146875355999E+17, float unit.Scale)
        Assert.AreEqual(
            set [ BaseUnit.create "time" "s", 1 ],
            unit |> Unit.baseUnits)
