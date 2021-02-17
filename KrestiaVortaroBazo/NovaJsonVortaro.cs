using System.Collections.Generic;

namespace KrestiaVortaroBazo {
   public class NovaJsonVortaro {
      public List<Substantivo> Substantivoj { get; set; } = null!;
      public List<Rekordo> Rekordoj { get; set; } = null!;
      public List<Verbo> Verboj { get; set; } = null!;
      public List<Modifanto> Modifantoj { get; set; } = null!;
      public List<VortaraVorto> SpecialajVortoj { get; set; } = null!;
      public List<NovaKategorio> Kategorioj { get; set; } = null!;
   }

   public class VortaraVorto {
      public string Vorto { get; set; } = null!;
      public string Signifo { get; set; } = null!;
      public string Gloso { get; set; } = null!;
      public List<string> Radikoj { get; set; } = null!;
      public string? Noto { get; set; }
   }

   public class Substantivo : VortaraVorto {
      public string? PlenaFormo { get; set; }
   }

   public class Rekordo : VortaraVorto {
      public List<string> ValuajTipoj { get; } = null!;
      public List<string> ValuajSignifoj { get; } = null!;
   }

   public class Verbo : VortaraVorto {
      public List<string?> ArgumentajNotoj { get; set; } = null!;
      public string FrazaSignifo { get; set; } = "";
      public string? PlenaFormo { get; set; }
   }

   public class Modifanto : VortaraVorto {
      public List<string> ModifeblajTipoj { get; set; } = null!;
      public List<string> AldonaĵajTipoj { get; set; } = null!;
      public List<string?> AldonaĵajNotoj { get; set; } = null!;
   }

   public class NovaKategorio {
      public string Nomo { get; set; } = null!;
      public List<string> Vortoj { get; set; } = null!;
   }
}