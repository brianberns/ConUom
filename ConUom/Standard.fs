namespace ConUom

module Standard =

    let centi = Quotient (Var, Const 100m)

    module SI =

        let meter = BaseUnit (Length, "m")
        let m = meter

        let centimeter = DerivedUnit (meter, centi, "cm")
        let cm = centimeter

    module Imperial =

        open SI

        let inch = DerivedUnit (centimeter, Product (Var, Const 2.54m), "in")

        let feet = DerivedUnit (inch, Product (Var, Const 12m), "ft")
        let ft = feet

        let gallon = DerivedUnit (inch ^ 3, Product (Var, Const 231m), "gal")
        let gal = gallon
