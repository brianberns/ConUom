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
            var sqm = m ^ 2;

            /*
            AssertEq(
                5.08m @ cm,
                (2 @ inch) => cm)
            AssertEq(
                2 @ inch,
                (5.08m @ cm) => inch)
            */
        }
    }
}
