namespace ConUom

open MathNet.Numerics

module Standard =

    let centi = Quotient (Var, Const 100N)

    module SI =

        let meter = BaseUnit (Length, "m")
        let m = meter

        let centimeter = DerivedUnit (meter, centi, "cm")
        let cm = centimeter

    module Imperial =

        open SI

        let inch = DerivedUnit (centimeter, Product (Var, Const (254N/100N)), "in")

        let feet = DerivedUnit (inch, Product (Var, Const 12N), "ft")
        let ft = feet

        let gallon = DerivedUnit (inch ^ 3, Product (Var, Const 231N), "gal")
        let gal = gallon
