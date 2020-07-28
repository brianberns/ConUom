using System;
using System.Linq;
using System.Net;

using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ConUom.CS.Test
{
    [TestClass]
    public class Tests
    {
        static void AssertEq(Measurement measA, Measurement measB)
        {
            Assert.AreEqual(measA.Value, measB.Value, $"{measA.Value} vs. {measB.Value}");
            Assert.IsTrue(measA.Unit.BaseUnits.SequenceEqual(measB.Unit.BaseUnits));
            Assert.AreEqual(measA.Unit.Scale, measB.Unit.Scale);
        }

        [TestMethod]
        public void Area()
        {
            // define the meter as a base unit
            var m = new Unit("m");

            // define an inch in terms of centimeters
            var cm = 0.01m * m;
            var inch = 2.54m * cm;

            // define a square yard in terms of inches
            var ft = 12 * inch;
            var yd = 3 * ft;
            var sqyd = yd ^ 2;

            // measure an area of 8 square yards
            var areaSqYd = sqyd.Measure(8);

            // how much is that area in square meters?
            var areaSqM = areaSqYd.ConvertTo(m ^ 2);
            Console.WriteLine($"{areaSqYd.Value} square yards = {(double)areaSqM.Value} square meters");   // Output: 8 square yards = 6.68901888 square meters
            AssertEq((m ^ 2).Measure(6.68901888m), areaSqM);
            AssertEq(areaSqYd, areaSqM.ConvertTo(yd ^ 2));
        }

        [TestMethod]
        public void WmbrInSmoots()
        {
            // download and parse standard units
            using var client = new WebClient();
            var str = client.DownloadString("https://frinklang.org/frinkdata/units.txt");
            var success = Frink.TryParse(str, out UnitLookup lookup);
            Assert.IsTrue(success);

            var smoot = lookup["smoot"];                          // height of Oliver Smoot
            var wmbr = lookup["megahertz"].Measure(88.1m);        // frequency of radio station WMBR
            var c = lookup["c"].Measure(1);                       // speed of light (as a measurement)
            var wavelength = c / wmbr;                            // WMBR's wavelength
            var nSmoots = wavelength.ConvertTo(smoot);            // wavelength of WMBR in smoots
            Assert.AreEqual(2.0, (double)nSmoots.Value, 0.001);   // WMBR wavelength = 2 smoots!
        }
    }
}
