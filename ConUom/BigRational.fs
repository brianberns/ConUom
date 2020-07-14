namespace ConUom

open System
open System.Numerics

open MathNet.Numerics

[<AutoOpen>]
module BigRationalExt =

    type BigRational with

        /// Creates a BigRational from a decimal.
        /// https://docs.microsoft.com/en-us/dotnet/api/system.decimal.getbits
        static member FromDecimal(n : decimal) =
            let parts = Decimal.GetBits(n)
            assert(parts.Length = 4)
            let shift value n =
                (value |> uint |> bigint) <<< n
            let lo =  shift parts.[0]  0
            let mid = shift parts.[1] 32
            let hi =  shift parts.[2] 64
            let sign = if parts.[3] &&& 0x80000000 = 0 then 1I else -1I
            let scale = (parts.[3] >>> 16) &&& 0x7F
            BigRational.FromBigIntFraction(
                sign * (lo + mid + hi),
                BigInteger.Pow(10I, scale))

    /// Creates a BigRational from a decimal.
    let dec = BigRational.FromDecimal
