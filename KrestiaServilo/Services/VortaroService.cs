using KrestiaVortaro;

namespace KrestiaServilo.Services {
   public class VortaroService : IVortaroService {
      public Vortaro Instanco { get; }

      public VortaroService() {
         Instanco = Vortaro.KreiVortaronDe(
            "https://raw.githubusercontent.com/Sam647254/Krestia/master/novaVortaro.json").Result;
      }
   }
}