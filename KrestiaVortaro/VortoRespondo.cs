using System;
using System.Collections.Generic;

namespace KrestiaVortaro {
   public class VortoRespondo {
      private List<string>? _kategorioj;
      private readonly string? _noto;
      private readonly List<string>? _radikoj;

      public string Vorto { get; set; }
      
      public string? Bazo { get; set; }

      public List<string>? Radikoj {
         get => _radikoj ?? new List<string>();
         init => _radikoj = value;
      }

      public string? Signifo { get; init; }

      public string? Gloso { get; init; }

      public string? Noto {
         get => _noto ?? "";
         init => _noto = value;
      }

      public List<string>? Kategorioj {
         get => _kategorioj ?? new List<string>();
         set => _kategorioj = value;
      }

      public string? Vorttipo { get; init; }

      public IEnumerable<string>? Silaboj { get; init; }

      public IDictionary<string, string>? InflektitajFormoj { get; init; }

      public IEnumerable<string?>? Ujoj { get; init; }
      
      public string? FrazaSignifo { get; init; }
      
      public string? Sintakso { get; init; }
      
      public IEnumerable<string>? ModifeblajVorttipoj { get; init; }

      internal VortoRespondo(string vorto) {
         Vorto = vorto;
      }
   }
}