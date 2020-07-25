# ConUom
ConUom allows you to define and convert between units of measure in C# or F#.
## Example
Let's start by defining the meter as a base unit of length:
```csharp
var m = new Unit("Length", "m");
```
We can then derive new units by scaling exiting units up or down. For example, a centimeter is 1/100<sup>th</sup> of a meter:
```csharp
var cm = 0.01m * m;
```
Note that the `m` at the end of `0.01m` indicates a fixed-point decimal literal (rather than floating-point). We use exact decimals to avoid rounding errors.

We can convert from metric to U.S. units by defining the inch as exactly 2.54 centimeters:
```csharp
var inch = 2.54m * cm;
```
<!--stackedit_data:
eyJoaXN0b3J5IjpbMTM0NjQ4MTg2N119
-->