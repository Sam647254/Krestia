using System.Collections.Generic;

namespace KrestiaVortaro {
   public class NovaVortaro {
      public List<Substantivo> Substantivoj { get; set; } = null!;
      public List<Rekordo> Rekordoj { get; set; } = null!;
      public List<Verbo> Verboj { get; set; } = null!;
      public List<Modifanto> Modifantoj { get; set; } = null!;
   }

   public class VortaraVorto {
      public string Vorto { get; set; } = null!;
      public string Signifo { get; set; } = null!;
      public string Gloso { get; set; } = null!;
      public List<string> Radikoj { get; set; } = null!;
      public string Noto { get; set; } = null!;
   }

   public class Substantivo : VortaraVorto {
      public string? PlenaFormo { get; set; }
   }

   public class Rekordo : VortaraVorto {
      public List<string> ValuajTipoj { get; } = null!;
      public List<string> ValuajSignifoj { get; } = null!;
   }

   public class Verbo : VortaraVorto {
      public List<string?> ArgumentajNotoj { get; } = null!;
      public string? PlenaFormo { get; set; }
   }

   public class Modifanto : VortaraVorto {
      public List<string> ModifeblajTipoj { get; } = null!;
      public List<string> AldonaĵajTipoj { get; } = null!;
      public List<string?> AldonaĵajNotoj { get; } = null!;
   }
}