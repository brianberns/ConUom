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
Instead of defining our own units from scratch, we can download and use units defined in the [Frink](https://frinklang.org/frinkdata/units.txt) language:
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
<!--stackedit_data:
eyJoaXN0b3J5IjpbLTE5OTAzMTUxMDUsLTY1NTk3MTYzNSw2Nz
YxNzc0NDUsOTUzODY0MzA4XX0=
-->