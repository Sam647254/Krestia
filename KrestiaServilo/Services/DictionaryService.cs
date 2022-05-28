using KrestiaVortaro;

namespace KrestiaServilo.Services; 

public class DictionaryService : IDictionaryService {
   public Vortaro Instance { get; }

   public DictionaryService() {
      Instance = Vortaro.CreateFromResource();
   }
}