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
               new Vorto(id: 0, "kunaa", "kunaa", new List<int>(), "water", "water")
            },
            Kategorioj = new List<VortaraKategorio>()
         };
      }

      [Test]
      public void AldoniVortojn() {
         const string eniro = @"lirano|night (peaceful)|night||
mikaa|day|day||";
         var vicoj = eniro.Split(separator: '\n');
         var novajVortoj = Agoj.AldoniVortojn(_vortaro, vicoj).ToList();
         Assert.AreEqual(expected: 2, novajVortoj.Count);
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