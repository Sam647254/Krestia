using System.Threading.Tasks;
using KrestiaServilo.Services;

namespace KrestiaServilo.Testo.Testiloj {
   public class TestaBlissFonto : IBlissFonto {
      public Task<string> AlportiBlissimbolon(int id) {
         return Task.FromResult("");
      }
   }
}