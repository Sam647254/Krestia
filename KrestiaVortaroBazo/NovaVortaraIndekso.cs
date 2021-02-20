using System.Collections.Generic;
using System.Linq;
using Newtonsoft.Json;

namespace KrestiaVortaroBazo {
   public class NovaVortaraIndekso {
      public Dictionary<string, VortaraVorto> Indekso { get; }

      private readonly NovaJsonVortaro _vortaro;

      public NovaVortaraIndekso(string eniro) {
         _vortaro = JsonConvert.DeserializeObject<NovaJsonVortaro>(eniro)!;
         
         Indekso = new Dictionary<string, VortaraVorto>();
         
         var vortoj = _vortaro.Substantivoj
            .Concat<VortaraVorto>(_vortaro.Verboj)
            .Concat(_vortaro.Rekordoj)
            .Concat(_vortaro.Modifantoj)
            .Concat(_vortaro.SpecialajVortoj);
         foreach (var vorto in vortoj) {
            Indekso.Add(vorto.Vorto, vorto);
         }
      }

      public string IgiEnJson() {
         return JsonConvert.SerializeObject(_vortaro, Formatting.Indented);
      }
   }
}