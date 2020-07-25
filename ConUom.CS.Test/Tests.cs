using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using MathNet.Numerics;

namespace ConUom.CS.Test
{
    [TestClass]
    public class Tests
    {
        static void AssertEq(Measurement measA, Measurement measB)
        {
            Assert.AreEqual(measA.Value, measB.Value);
            Assert.IsTrue(measA.Unit.BaseUnits.SequenceEqual(measB.Unit.BaseUnits));
            Assert.AreEqual(measA.Unit.Scale, measB.Unit.Scale);
        }

        [TestMethod]
        public void Length()
        {
            var centi = BigRational.FromIntFraction(1, 100);
            var m = new Unit("Length", "m");
            var cm = centi * m;
            var inch = 2.54m * cm;

            AssertEq(
                cm.Measure(5.08m),
                inch.Measure(2).ConvertTo(cm));
            AssertEq(
                inch.Measure(2),
                cm.Measure(5.08m).ConvertTo(inch));
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
