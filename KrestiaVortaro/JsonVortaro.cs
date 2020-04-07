using System.Collections.Generic;
using System.Collections.Immutable;

namespace KrestiaVortaro {
   public class JsonVortaro {
      public IEnumerable<Vorto> Vortoj { get; set; }
      public IEnumerable<VortaraKategorio> Kategorioj { get; set; }
   }

   public class VortaraKategorio {
      public int Id { get; set; }
      public string Nomo { get; set; }
      public IEnumerable<int> Vortoj { get; set; }
   }
}