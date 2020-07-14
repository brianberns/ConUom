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

    let percent = Unit.createEmpty (1N/100N)

    module SI =

        let meter = Unit.createBase Length "m"
        let m = meter

        let centimeter = centi @@ meter
        let cm = centimeter

        let second = Unit.createBase Time "s"
        let sec = second
        let s = second

        let minute = 60N @@ second
        let min = minute

        let hour = 60N @@ minute
        let hr = hour

        let kilogram = Unit.createBase Mass "kg"
        let kg = kilogram

        let gram = milli @@ kg
        let g = gram

        let cc = 1N @@ (cm^3)
        let liter = 1000N @@ cc
        let magnum = (3N/2N) @@ liter

        let density =
            Unit.div gram cc
                |> Unit.create
        let water = density 1N
        let alcohol = density (dec 0.7893m)

    module Imperial =

        open SI

        let inch = (dec 2.54m) @@ centimeter

        let foot = 12N @@ inch
        let ft = foot

        let mile = 5280N @@ ft

        let gallon = 231N @@ (inch^3)
        let gal = gallon

        let quart = (1N/4N) @@ gal
        let qt = quart

        let pint = (1N/2N) @@ qt
        let pt = pint

        let fluidounce = (1N/16N) @@ pint
        let floz = fluidounce

        let pound = (dec 0.45359237m) @@ kg 
        let lb = pound
