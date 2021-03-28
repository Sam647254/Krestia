using System.IO;
using System.Text;
using System.Threading.Tasks;
using KrestiaServilo.Services;
using KrestiaVortilo;
using Microsoft.AspNetCore.Mvc;

namespace KrestiaServilo.Controllers {
   [ApiController]
   [Route("api")]
   public class VortoController : ControllerBase {
      private readonly IVortaroService _vortaroService;

      public VortoController(IVortaroService vortaroService) {
         _vortaroService = vortaroService;
      }

      [HttpGet("vorto/{vorto}")]
      public IActionResult Get(string vorto) {
         var rezulto = _vortaroService.Instanco.Vorto(vorto);
         if (rezulto == null)
            return NotFound();
         return Ok(rezulto);
      }

      [HttpGet("trovi/{peto}")]
      public ActionResult Trovi(string peto) {
         var vortaro = _vortaroService.Instanco;
         var rezulto = vortaro.TroviVortojn(peto);
         return Ok(rezulto);
      }

      [HttpGet("vortlisto/alfabeta")]
      public ActionResult AlfabetaListo() {
         return Ok(_vortaroService.Instanco.Vortlisto);
      }

      [HttpGet("vortlisto/tipo")]
      public ActionResult TipaVortlisto() {
         return Ok(_vortaroService.Instanco.TipaVortlisto);
      }

      [HttpGet("vortlisto/kategorioj")]
      public ActionResult KategoriaVortlisto() {
         return Ok(_vortaroService.Instanco.KategoriaVortlisto);
      }

      [HttpGet("gloso/{vorto}")]
      public ActionResult Gloso(string vorto) {
         var rezulto = _vortaroService.Instanco.TroviGlosanSignifon(vorto);
         if (rezulto == null) return NotFound();
         return Ok(new {gloso = rezulto});
      }

      [HttpPost("malinflekti")]
      public ActionResult Malinflekti([FromBody] Peto peto) {
         var rezulto = Sintaksanalizilo2.prepariEniron(peto.Eniro, false);
         return rezulto.IsOk ? Ok(rezulto.ResultValue) : UnprocessableEntity(rezulto.ErrorValue);
      }
   }
}