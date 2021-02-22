using System;
using System.Collections.Generic;
using KrestiaServilo.Services;
using KrestiaVortaro;

namespace KrestiaServilo.Testo.Testiloj {
   internal class TestaVortaro : IVortaroService {
      public Vortaro Instanco { get; }

      internal TestaVortaro() {
         throw new NotImplementedException();
      }
   }
}