using System;
using System.Net.Http;
using System.Threading.Tasks;

using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ConUom.CS.Test
{
    [TestClass]
    public class Tests
    {
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
            Assert.AreEqual((m ^ 2).Measure(6.68901888m), areaSqM);
            Assert.AreEqual(areaSqYd, areaSqM);
        }

        [TestMethod]
        public async Task WmbrInSmoots()
        {
            // download and parse standard units
            using var client = new HttpClient();
            var str = await client.GetStringAsync("https://frinklang.org/frinkdata/units.txt");
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
