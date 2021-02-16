using System.Collections.Generic;
using System.Linq;
using Newtonsoft.Json;

namespace KrestiaVortaroBazo {
   public class NovaVortaraIndekso {
      public Dictionary<string, VortaraVorto> Indekso { get; }

      public NovaVortaraIndekso(string eniro) {
         var vortaro = JsonConvert.DeserializeObject<NovaJsonVortaro>(eniro);
         
         Indekso = new Dictionary<string, VortaraVorto>();
         
         var vortoj = vortaro!.Substantivoj
            .Concat<VortaraVorto>(vortaro.Verboj)
            .Concat(vortaro.Rekordoj)
            .Concat(vortaro.Modifantoj)
            .Concat(vortaro.SpecialajVortoj);
         foreach (var vorto in vortoj) {
            Indekso.Add(vorto.Vorto, vorto);
         }
      }
   }
}