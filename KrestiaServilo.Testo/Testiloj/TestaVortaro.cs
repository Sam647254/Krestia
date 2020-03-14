using System.Collections.Generic;
using KrestiaServilo.Services;
using KrestiaVortaro;

namespace KrestiaServilo.Testo.Testiloj {
   internal class TestaVortaro : IVortaroService {
      public Vortaro Instanco { get; }

      internal TestaVortaro() {
         Instanco = Vortaro.KreiVortaronDe(new JsonVortaro {
            Vortoj = new List<Vorto> {
               new Vorto(id: 0, "kunaa", "kunaa", new int[] { }, "water", "water"),
            },
         });
      }
   }
}