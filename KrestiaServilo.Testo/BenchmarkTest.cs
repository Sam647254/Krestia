using KrestiaServilo.Services;
using NUnit.Framework;

namespace KrestiaServilo.Testo; 

public class BenchmarkTest {
   [Test]
   public void BenchmarkDictionaryLoad() {
      var _ = new VortaroService();
      Assert.Pass();
   }
}