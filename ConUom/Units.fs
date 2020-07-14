namespace ConUom.Units

open ConUom
open MathNet.Numerics

[<AutoOpen>]
module General =

    let kilo  = 1000N
    let hecto =  100N
    let deka  =   10N
    let deci  = dec 0.1m
    let centi = dec 0.01m
    let milli = dec 0.001m

    let percent = Unit.createScale (1N/100N)

/// International System of Units.
namespace ConUom.Units.SI

open ConUom
open ConUom.Units

open MathNet.Numerics

module Length =

    let meter = Unit.createBase Length "m"
    let m = meter

    let centimeter = centi @@ meter
    let cm = centimeter

module Mass =

    let kilogram = Unit.createBase Mass "kg"
    let kg = kilogram

    let gram = milli @@ kg
    let g = gram

module Time =

    let second = Unit.createBase Time "s"
    let sec = second
    let s = second

    let minute = 60N @@ second
    let min = minute

    let hour = 60N @@ minute
    let hr = hour

module Volume =

    open Length

    let cc = 1N @@ (cm^3)
    let liter = 1000N @@ cc

module Density =

    open Mass
    open Volume

    let density = Unit.div gram cc
    let water = 1N @@ density

/// United States customary units.
namespace ConUom.Units.USCS

open ConUom
open MathNet.Numerics

module Length =

    open ConUom.Units.SI.Length

    let inch = 2.54m @@ centimeter

    let foot = 12N @@ inch
    let ft = foot

    let mile = 5280N @@ ft

module Mass =

    open ConUom.Units.SI.Mass

    let pound = 0.45359237m @@ kg 
    let lb = pound

module Volume =

    open Length

    let gallon = 231N @@ (inch^3)
    let gal = gallon

    let quart = 0.25m @@ gal
    let qt = quart

    let pint = 0.5m @@ qt
    let pt = pint

    let fluidounce = (1N/16N) @@ pint
    let floz = fluidounce

namespace ConUom.Units.Liquor

open ConUom

[<AutoOpen>]
module General =

    open ConUom.Units.SI.Density
    open ConUom.Units.SI.Volume

    let magnum = 1.5m @@ liter
    let alcohol = 0.7893m @@ density
