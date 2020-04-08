using System.Collections.Generic;
using System.Linq;
using KrestiaServilo.Controllers;
using KrestiaServilo.Testo.Testiloj;
using KrestiaVortaro;
using Microsoft.AspNetCore.Mvc;
using NUnit.Framework;

namespace KrestiaServilo.Testo {
   public class Tests {
      private readonly VortoController _controller = new VortoController(new TestaVortaro());

      [Test]
      public void Get() {
         var vorto = _controller.Get("kunaa");
         
         Assert.IsInstanceOf<OkObjectResult>(vorto);
         var respondo = ((OkObjectResult) vorto).Value as VortoRespondo;
         Assert.AreEqual("kunaa", respondo?.Vorto);
         Assert.AreEqual("water", respondo?.Signifo);
      }

      [Test]
      public void Trovi() {
         var respondo = _controller.Trovi("water");
         
         Assert.IsInstanceOf<OkObjectResult>(respondo);
         var rezulto = ((OkObjectResult) respondo).Value as VortoRezulto;
         Assert.AreEqual(1, rezulto?.Rezultoj.Count());
         Assert.AreEqual("kunaa", rezulto?.Rezultoj.First().Vorto);
      }

      [Test]
      public void TroviInflektitan() {
         var respondo = _controller.Trovi("kunarimia");
         
         Assert.IsInstanceOf<OkObjectResult>(respondo);
         var rezulto = (VortoRezulto) ((OkObjectResult) respondo).Value;
         Assert.AreEqual("kunaa", rezulto.MalinflektitaVorto);
      }

      [Test]
      public void TroviNeekzistan() {
         var respondo = _controller.Trovi("abc");
         
         Assert.IsInstanceOf<OkObjectResult>(respondo);
         var rezulto = (VortoRezulto) ((OkObjectResult) respondo).Value;
         Assert.IsNull(rezulto.MalinflektitaVorto);
         Assert.IsNull(rezulto.PlenigitaVorto);
         Assert.IsEmpty(rezulto.Rezultoj);
      }

      [Test]
      public void AlfabetaListo() {
         var respondo = _controller.AlfabetaListo();
         
         Assert.IsInstanceOf<OkObjectResult>(respondo);
         var rezulto = (((OkObjectResult) respondo).Value as IOrderedEnumerable<Vortaro.VortoKunSignifo>)!.ToList();
         Assert.AreEqual(2, rezulto?.Count);
         Assert.AreEqual("kresku", rezulto?[0].Vorto);
         Assert.AreEqual("kunaa", rezulto?[1].Vorto);
      }
   }
}