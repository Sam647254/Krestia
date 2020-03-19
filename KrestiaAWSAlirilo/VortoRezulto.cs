using System.Collections.Generic;

namespace KrestiaAWSAlirilo {
   public class VortoRezulto {
      public string? MalinflektitaVorto { get; set; }
      public string? PlenigitaVorto { get; set; }
      public IEnumerable<VortoRespondo> Rezultoj { get; set; }
      public string? Gloso { get; set; }
      public IEnumerable<string>? MalinflektaŜtupoj { get; set; }

      public VortoRezulto() {
         Rezultoj = new List<VortoRespondo>();
      }
   }
}