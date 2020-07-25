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
                inch.Measure(2).Convert(cm));
            AssertEq(
                inch.Measure(2),
                cm.Measure(5.08m).Convert(inch));
        }
    }
}
