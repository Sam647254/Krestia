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
         Assert.AreEqual(expected: 1, rezulto?.Rezultoj.Count());
         Assert.AreEqual("kunaa", rezulto?.Rezultoj.First().Vorto);
      }
   }
}