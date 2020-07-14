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
            let lo =  (bigint parts.[0]) <<<  0
            let mid = (bigint parts.[1]) <<< 32
            let hi =  (bigint parts.[2]) <<< 64
            let sign = if (parts.[3] &&& 0x80000000) = 0 then 1I else -1I
            let scale = (parts.[3] >>> 16) &&& 0x7F
            BigRational.FromBigIntFraction(
                sign * (lo + mid + hi),
                BigInteger.Pow(10I, scale))

    let dec = BigRational.FromDecimal

module Standard =

    let centi = dec 0.01m

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

    module Imperial =

        open SI

        let inch = Unit.create centimeter (dec 2.54m) "in"

        let foot = Unit.create inch 12N "ft"
        let ft = foot

        let mile = Unit.create ft 5280N "mile"

(*
    let kilo = Product (Var, Const 1000N)
    let hecto = Product (Var, Const 100N)
    let deka = Product (Var, Const 10N)
    let deci = Quotient (Var, Const 10N)
    let centi = Quotient (Var, Const 100N)
    let milli = Quotient (Var, Const 1000N)

    module SI =

        let meter = BaseUnit (Length, "m")
        let m = meter

        let centimeter = DerivedUnit (meter, centi, "cm")
        let cm = centimeter

        let kilogram = BaseUnit (Mass, "kg")
        let kg = kilogram

        let gram = DerivedUnit (kg, milli, "g")
        let g = gram

        let water = Unit.product gram cm^(-3)

    module Imperial =

        open SI

        let inch = DerivedUnit (centimeter, Product (Var, Expr.decimal 2.54m), "in")

        let foot = DerivedUnit (inch, Product (Var, Const 12N), "ft")
        let ft = foot

        let gallon = DerivedUnit (inch ^ 3, Product (Var, Const 231N), "gal")
        let gal = gallon

        let pound = DerivedUnit (kg, Product (Var, Expr.decimal 0.45359237m), "lb")
        let lb = pound
*)
