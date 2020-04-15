using System.IO;
using System.Threading.Tasks;

namespace KrestiaServilo.Services {
   public interface IBlissFonto {
      Task<string> AlportiBlissimbolon(int id);
   }
}