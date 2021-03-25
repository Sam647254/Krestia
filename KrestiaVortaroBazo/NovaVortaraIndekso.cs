using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Newtonsoft.Json;

namespace KrestiaVortaroBazo {
   public class NovaVortaraIndekso {
      public IImmutableDictionary<string, VortaraVorto> Indekso { get; private set; } = null!;
      public IEnumerable<NovaKategorio> Kategorioj => _vortaro.Kategorioj;

      private readonly NovaJsonVortaro _vortaro;

      public NovaVortaraIndekso(string eniro) {
         _vortaro = JsonConvert.DeserializeObject<NovaJsonVortaro>(eniro)!;
         KreiIndekson();
      }

      public NovaVortaraIndekso(NovaJsonVortaro vortaro) {
         _vortaro = vortaro;
         KreiIndekson();
      }

      public string IgiEnJson() {
         return JsonConvert.SerializeObject(_vortaro, Formatting.Indented);
      }

      private void KreiIndekson() {
         Indekso = _vortaro.Substantivoj
            .Concat<VortaraVorto>(_vortaro.Verboj)
            .Concat(_vortaro.Rekordoj)
            .Concat(_vortaro.Modifantoj)
            .Concat(_vortaro.SpecialajVortoj)
            .ToImmutableDictionary(v => v.Vorto, v => v);
      }
   }
}