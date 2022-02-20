using System.Collections.Generic;

namespace KrestiaVortaro {
   public class VortoRezulto {
      public string? MalinflektitaVorto { get; set; }
      public string? PlenigitaVorto { get; set; }
      public IEnumerable<Vortaro.WordWithMeaning> Rezultoj { get; set; }
      public string? Gloso { get; set; }
      public IEnumerable<string>? MalinflektajŜtupoj { get; set; }
      public IEnumerable<string>? GlosajVortoj { get; set; }
      public IEnumerable<IEnumerable<string>>? GlosajŜtupoj { get; set; }
      public IEnumerable<string>? BazajVortoj { get; set; }
      public double? NombroRezulto { get; set; }

      public VortoRezulto() {
         Rezultoj = new List<Vortaro.WordWithMeaning>();
      }
   }
}