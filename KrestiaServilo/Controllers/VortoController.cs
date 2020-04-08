using KrestiaServilo.Services;
using Microsoft.AspNetCore.Mvc;

namespace KrestiaServilo.Controllers {
   [ApiController]
   [Route("")]
   public class VortoController : ControllerBase {
      private readonly VortaroService _vortaroService;

      public VortoController(VortaroService vortaroService) {
         _vortaroService = vortaroService;
      }

      [HttpGet("vorto/{vorto}")]
      public ActionResult Get(string vorto) {
         return NotFound();
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
   }
}