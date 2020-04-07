using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace KrestiaVortaro {
   public class Vorto : IComparable<Vorto> {
      public string PlenaVorto { get; set; }

      public string BazaVorto { get; set; }

      public ImmutableList<int> Radikoj { get; }

      public string Signifo { get; }
      
      public string? Ujo1 { get; }
      
      public string? Ujo2 { get; }
      
      public string? Ujo3 { get; }

      public string GlosaSignifo { get; }

      public string? Noto { get; }

      public Vorto(string plenaVorto, string bazaVorto, IEnumerable<int> radikoj, string signifo,
         string glosaSignifo, string? ujo1 = null, string? ujo2 = null, string? ujo3 = null, string? noto = null) {
         PlenaVorto = plenaVorto;
         BazaVorto = bazaVorto;
         Radikoj = radikoj.ToImmutableList();
         Signifo = signifo;
         GlosaSignifo = glosaSignifo;
         Ujo1 = ujo1;
         Ujo2 = ujo2;
         Ujo3 = ujo3;
         Noto = noto;
      }

      public int CompareTo(Vorto other) {
         return string.Compare(PlenaVorto, other.PlenaVorto, StringComparison.Ordinal);
      }
   }
}