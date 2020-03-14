using KrestiaVortaro;

namespace KrestiaServilo.Services {
   public class VortaroService : IVortaroService {
      public Vortaro Instanco { get; }
      
      public VortaroService() {
         Instanco = Vortaro.KreiVortaron().Result;
      }
   }
}