using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading.Tasks;

namespace KrestiaVortaro {
   public class Vortaro {
      private const string VortaroUrl = "https://raw.githubusercontent.com/Sam647254/Krestia/master/vortaro.json";

      public static Vortaro Instanco {
         get {
            lock (Ŝloso) {
               return _vortaro ??= KreiVortaron().Result;
            }
         }
      }

      private static readonly object Ŝloso = new object();
      private static Vortaro? _vortaro;

      public ImmutableDictionary<string, Vorto> Indekso { get; private set; }
      public ImmutableDictionary<int, Vorto> IdIndekso { get; private set; }
      public ImmutableDictionary<string, ImmutableList<Vorto>> Kategorioj { get; private set; }

      private static async Task<Vortaro> KreiVortaron() {
         var jsonVortaro = await JsonVortaro.Alporti(VortaroUrl);
         return new Vortaro {
            Indekso = jsonVortaro.Vortoj.ToImmutableDictionary(v => v.PlenaVorto, v => v),
            IdIndekso = jsonVortaro.Vortoj.Select((v, i) => (v, i)).ToImmutableDictionary(p => p.i, p => p.v),
            Kategorioj = jsonVortaro.Kategorioj.ToImmutableDictionary(k => k.Nomo,
               k => k.Vortoj.Select(id => jsonVortaro.Vortoj[id]).ToImmutableList())
         };
      }
   }
}