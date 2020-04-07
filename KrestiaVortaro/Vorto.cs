using System.Collections.Generic;
using System.Collections.Immutable;

namespace KrestiaVortaro {
   public class Vorto {
      public int Id { get; }

      public string PlenaVorto { get; }

      public string BazaVorto { get; }

      public ImmutableList<int> Radikoj { get; }

      public string Signifo { get; }

      public string GlosaSignifo { get; }

      public string? Noto { get; }

      public Vorto(int id, string plenaVorto, string bazaVorto,
         IEnumerable<int> radikoj, string signifo, string glosaSignifo,
         string? noto = null) {
         Id = id;
         PlenaVorto = plenaVorto;
         BazaVorto = bazaVorto;
         Radikoj = radikoj.ToImmutableList();
         Signifo = signifo;
         GlosaSignifo = glosaSignifo;
         Noto = noto;
      }
   }
}