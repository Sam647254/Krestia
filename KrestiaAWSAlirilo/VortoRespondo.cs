using System.Collections.Generic;

namespace KrestiaAWSAlirilo {
   public class VortoRespondo {
      private List<string>? _kategorioj;
      private string? _noto;
      private List<string>? _radikoj;

      public string Vorto { get; set; }

      public List<string> Radikoj {
         get => _radikoj ?? new List<string>();
         set => _radikoj = value;
      }

      public string Signifo { get; set; }
      
      public string Gloso { get; set; }

      public string Noto {
         get => _noto ?? "";
         set => _noto = value;
      }

      public List<string> Kategorioj {
         get => _kategorioj ?? new List<string>();
         set => _kategorioj = value;
      }
      
      public string? Vorttipo { get; set; }
      
      public IEnumerable<string>? Silaboj { get; set; }
   }
}