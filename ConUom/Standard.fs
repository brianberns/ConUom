namespace ConUom

open MathNet.Numerics

module Standard =

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
