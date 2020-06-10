using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace KrestiaVortaro.Testo {
   public class AgojTestoj {
      private static JsonVortaro _vortaro;

      [SetUp]
      public void Setup() {
         _vortaro = new JsonVortaro {
            Vortoj = new List<Vorto> {
               new Vorto("kunaa", "kunaa", new List<string>(), "water", "water"),
            },
            Kategorioj = new List<VortaraKategorio>(),
         };
      }

      [Test]
      public void NevalideAldoniVorton() {
         foreach (var vico in new[] {
            "lirane|night (peaceful)|night||", // ne estas bazo
            "lirane", // ne sufiĉe de partoj
            "abc", // nevalida vorto
            "lirane|night (peaceful)|night|abc|", // nevalida radiko
            "nitrit|read|read||", // nevalida malplenigita formo 
            "kunaa|water|water||", // jam ekzistas
         }) {
            Assert.Throws<InvalidOperationException>(() => {
               var unused = Agoj.AldoniVortojn(_vortaro, new[] {vico}).ToList();
            });
         }
      }
   }
}