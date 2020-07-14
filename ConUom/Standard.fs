namespace ConUom

open MathNet.Numerics

[<AutoOpen>]
module BigRationalExt =

    open System
    open System.Numerics

    type BigRational with

        /// https://docs.microsoft.com/en-us/dotnet/api/system.decimal.getbits
        static member FromDecimal(n : decimal) =
            let parts = Decimal.GetBits(n)
            assert(parts.Length = 4)
            let toBigInt = uint >> bigint
            let lo =  (toBigInt parts.[0]) <<<  0
            let mid = (toBigInt parts.[1]) <<< 32
            let hi =  (toBigInt parts.[2]) <<< 64
            let sign = if (parts.[3] &&& 0x80000000) = 0 then 1I else -1I
            let scale = (parts.[3] >>> 16) &&& 0x7F
            BigRational.FromBigIntFraction(
                sign * (lo + mid + hi),
                BigInteger.Pow(10I, scale))

    let dec = BigRational.FromDecimal

module Standard =

    let kilo  =  1000N
    let hecto =   100N
    let deka  =    10N
    let deci  = dec 0.1m
    let centi = dec 0.01m
    let milli = dec 0.001m

    module SI =

        let meter = Unit.createBase Length "m"
        let m = meter

        let centimeter = Unit.create meter centi "cm"
        let cm = centimeter

        let second = Unit.createBase Time "s"
        let sec = second
        let s = second

        let minute = Unit.create second 60N "min"
        let min = minute

        let hour = Unit.create minute 60N "hr"
        let hr = hour

        let kilogram = Unit.createBase Mass "kg"
        let kg = kilogram

        let gram = Unit.create kg milli "g"
        let g = gram

        let water = Unit.mult gram cm^(-3)

    module Imperial =

        open SI

        let inch = Unit.create centimeter (dec 2.54m) "in"

        let foot = Unit.create inch 12N "ft"
        let ft = foot

        let mile = Unit.create ft 5280N "mile"

        let gallon = Unit.create (inch^3) 231N "gal"
        let gal = gallon

        let pound = Unit.create kg (dec 0.45359237m) "lb"
        let lb = pound
