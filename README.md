# ConUom
ConUom is a .NET Core library that allows you to define units of measure and then convert between them in C# or F#.
## C# example
Let's start by defining the meter as a base unit of length:
```csharp
var m = new Unit("Length", "m");
```
We can then derive new units in terms of existing units. For example, a centimeter is 1/100<sup>th</sup> of a meter:
```csharp
var cm = 0.01m * m;
```
Note that the `m` at the end of `0.01m` indicates a fixed-point decimal literal (rather than floating-point). We use exact values to avoid rounding errors.

We can convert from metric to U.S. units by defining the inch as exactly 2.54 centimeters:
```csharp
var inch = 2.54m * cm;
```
Of course, a foot is then 12 inches, and a yard is 3 feet. We can even define a square yard via the `^` operator:
```csharp
var ft = 12 * inch;
var yd = 3 * ft;
var sqyd = yd ^ 2;
```
Note that all of the above values are of type `Unit`, and they're all based on the Length dimension we defined at the top. Now let's measure out exactly 8 square yards:
```csharp
var areaSqYd = sqyd.Measure(8);
```
This value has type `Measurement`. How many square meters are in 8 square yards? We can find out by converting units:
```csharp
var areaSqM = areaSqYd.ConvertTo(m ^ 2);

Console.WriteLine($"{areaSqYd.Value} square yards = {(double)areaSqM.Value} square meters");
// Output: 8 square yards = 6.68901888 square meters
```
## Using standard units
Instead of defining our own units from scratch, we can download a large collection of standardized units maintained by [Frink](https://frinklang.org/frinkdata/units.txt):
```csharp
using var client = new WebClient();
var str = client.DownloadString("https://frinklang.org/frinkdata/units.txt");
var success = Frink.TryParse(str, out UnitLookup lookup);
Assert.IsTrue(success);
```
For example, MIT's favorite unit of length is the "[smoot](https://en.wikipedia.org/wiki/Smoot)". We can use Frink's definition like this:
```csharp
var smoot = lookup["smoot"];
```
The school's radio station, WMBR, broadcasts at a frequency of 88.1 MHz:
```csharp
var wmbr = lookup["megahertz"].Measure(88.1m);
```
We can convert this frequency to a wavelength using the speed of light:
```csharp
var c = lookup["c"].Measure(1);
var wavelength = c / wmbr;
```
So, how long is WMBR's wavelength in smoots? That's easy to calculate:
```csharp
var nSmoots = wavelength.ConvertTo(smoot);
Console.WriteLine((double)nSmoots.Value);
// Output: 1.999568447856973
```
 Almost exactly 2!
## F# example
ConUom is written in F# and there are several advantages to using its F# API as well. To start with, we can parse the Frink data file, just like in C#:
```fsharp
use client = new WebClient()
let lookup, msgOpt =
    client.DownloadString("https://frinklang.org/frinkdata/units.txt")
        |> Frink.parse
msgOpt |> Option.iter Assert.Fail
```
Note that this version of the parser returns an `Option<string>` that contains an error message if the parse fails, in addition to the same `UnitLookup` we saw earlier.
This lookup can then be accessed using F#'s dynamic `?` operator:
```fsharp
let km = lookup?km
let megaparsec = lookup?megaparsec
let s = lookup?s
let gigayear = lookup?gigayear   // billions of years
```
Note that we can look up "gigayear" even though it is not explicitly defined in the Frink file, because "giga" is a known prefix (meaning 1 billion) and "year" is a known unit.
Let's use these units to calculate the age of the universe, shall we? This can be done easily using the Hubble constant, which is the rate at which the universe is expanding. One possible value for this constant is 
<!--stackedit_data:
eyJoaXN0b3J5IjpbLTExODU2MDM4NzMsLTE4MjQ2OTE5NTUsLT
E5OTAzMTUxMDUsLTY1NTk3MTYzNSw2NzYxNzc0NDUsOTUzODY0
MzA4XX0=
-->