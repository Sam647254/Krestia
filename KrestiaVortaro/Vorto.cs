using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace KrestiaVortaro {
   public class Vorto : IComparable<Vorto> {
      public string PlenaVorto { get; set; }

      public string BazaVorto { get; set; }

      public ImmutableList<string> Radikoj { get; }

      public string Signifo { get; set; }
      
      public string? Ujo1 { get; set; }
      
      public string? Ujo2 { get; set; }
      
      public string? Ujo3 { get; set; }

      public string GlosaSignifo { get; }

      public string? Noto { get; }
      
      public List<long>? Blissimbolo { get; set; }

      public Vorto(string plenaVorto, string bazaVorto, IEnumerable<string> radikoj, string signifo,
         string glosaSignifo, string? ujo1 = null, string? ujo2 = null, string? ujo3 = null, string? noto = null,
         IEnumerable<long>? blissimbolo = null) {
         PlenaVorto = plenaVorto;
         BazaVorto = bazaVorto;
         Radikoj = radikoj.ToImmutableList();
         Signifo = signifo;
         GlosaSignifo = glosaSignifo;
         Ujo1 = ujo1;
         Ujo2 = ujo2;
         Ujo3 = ujo3;
         Noto = noto;
         Blissimbolo = blissimbolo?.ToList();
      }

      public int CompareTo(Vorto other) {
         return string.Compare(PlenaVorto, other.PlenaVorto, StringComparison.Ordinal);
      }
   }
}