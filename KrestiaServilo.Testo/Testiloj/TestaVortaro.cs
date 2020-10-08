﻿using System;
using System.Collections.Generic;
using KrestiaServilo.Services;
using KrestiaVortaro;

namespace KrestiaServilo.Testo.Testiloj {
   internal class TestaVortaro : IVortaroService {
      public Vortaro Instanco { get; }

      internal TestaVortaro() {
         Instanco = Vortaro.KreiVortaronDe(new JsonVortaro {
            Vortoj = new List<Vorto> {
               new Vorto( "kuna", "kuna", new string[] { }, "water", "water"),
               new Vorto("kreski", "kreski", new string[] { }, "flame", "flame"),
            },
         });
      }
   }
}