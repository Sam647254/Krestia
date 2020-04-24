using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace KrestiaVortaro.Testo {
   public class TimeranTestoj {
      private static JsonVortaro _vortaro;

      [SetUp]
      public void Setup() {
         _vortaro = new JsonVortaro {
            Vortoj = new List<Vorto> {
               new Vorto("ato", "ato", new string[0], "", "", blissimbolo: new[] {123L}),
               new Vorto("renkaa", "renkaa", new string[0], "", "", blissimbolo: new[] {124L}),
               new Vorto("bit", "bi", new string[0], "", "", blissimbolo: new[] {125L}),
               new Vorto("hen", "hen", new string[0], "", ""),
               new Vorto("val", "val", new string[0], "", ""),
            },
            Kategorioj = new VortaraKategorio[0],
         };
      }

      [Test]
      public void Test1() {
         const string eniro = "hen atowa";
         const string eliro = "hen lokokupilo 123 predikativoEsti";
         Assert.AreEqual(eliro, Agoj.KonvertiEnTimeranTxt(_vortaro, new[] {eniro}).First());
      }

      [Test]
      public void Test2() {
         const string eniro = "hen bitre val renka";
         const string eliro = "hen lokokupilo 125 transitivaVerbo progresivo va malantaŭModifanto 124 difinito";
         Assert.AreEqual(eliro, Agoj.KonvertiEnTimeranTxt(_vortaro, new[] {eniro}).First());
      }
      
      [Test]
      public void Test3() {
         const string eniro = "hen var bitre renkave";
         const string eliro = "hen lokokupilo va antaŭModifanto 125 transitivaVerbo progresivo 124 pluraNombro";
         Assert.AreEqual(eliro, Agoj.KonvertiEnTimeranTxt(_vortaro, new[] {eniro}).First());
      }
   }
}