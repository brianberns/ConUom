using System;
using System.Linq;

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
            // define the meter as a base unit of length
            var m = new Unit("Length", "m");

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
        public void ParseUrl()
        {
            using var client = new System.Net.WebClient();
            var str = client.DownloadString("https://frinklang.org/frinkdata/units.txt");
            var success = Frink.TryParse(str, out UnitLookup lookup);
            Assert.IsTrue(success);

            var liter = lookup["liter"];
            var percent = lookup["percent"];
            var floz = lookup["floz"];
            var gal = lookup["gal"];
            var water = lookup["water"];
            var alcohol = lookup["alcohol"];
            var proof = lookup["proof"];

            var beer = (12 * floz) * (3.2m * percent) * (water / alcohol);
            var magnum = liter.Measure(1.5m) * percent.Measure(13.5m);
            Assert.AreEqual(
                14.074492562524341,
                (double)magnum.ConvertTo(beer).Value);

            var junglejuice = liter.Measure(1.75m) * proof.Measure(190) / gal.Measure(5);
            Assert.AreEqual(
                8.78372074090843481138500000,
                (double)junglejuice.ConvertTo(percent).Value);

            Assert.AreEqual(
                10.83279809499848,
                (double)(5 * floz.Measure(12) * junglejuice).ConvertTo(beer).Value);
        }
    }
}
