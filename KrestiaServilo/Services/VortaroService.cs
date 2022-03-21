using KrestiaVortaro;

namespace KrestiaServilo.Services; 

public class VortaroService : IVortaroService {
   public Vortaro Instance { get; }

   public VortaroService() {
      Instance = Vortaro.KreiVortaronDe(
         "https://raw.githubusercontent.com/Sam647254/Krestia/master/novaVortaro.json").Result;
   }
}