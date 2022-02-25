using KrestiaServilo.Services;
using Microsoft.AspNetCore.Mvc;

namespace KrestiaServilo.Controllers {
   [ApiController]
   [Route("api")]
   public class VortoController : ControllerBase {
      private readonly IVortaroService _dictionaryService;

      public VortoController(IVortaroService dictionaryService) {
         _dictionaryService = dictionaryService;
      }

      [HttpGet("vorto/{vorto}")]
      public IActionResult Get(string vorto) {
         var rezulto = _dictionaryService.Instance.Vorto(vorto);
         if (rezulto == null)
            return NotFound();
         return Ok(rezulto);
      }

      [HttpGet("trovi/{peto}")]
      public ActionResult Trovi(string peto) {
         var vortaro = _dictionaryService.Instance;
         var rezulto = vortaro.TroviVortojn(peto);
         return Ok(rezulto);
      }

      [HttpGet("vortlisto/alfabeta")]
      public ActionResult AlfabetaListo() {
         return Ok(_dictionaryService.Instance.WordList);
      }

      [HttpGet("vortlisto/tipo")]
      public ActionResult TipaVortlisto() {
         return Ok(_dictionaryService.Instance.WordListByType);
      }

      [HttpGet("vortlisto/kategorioj")]
      public ActionResult KategoriaVortlisto() {
         return Ok(_dictionaryService.Instance.KategoriaVortlisto);
      }

      [HttpGet("gloso/{vorto}")]
      public ActionResult Gloso(string vorto) {
         var rezulto = _dictionaryService.Instance.TroviGlosanSignifon(vorto);
         if (rezulto == null) return NotFound();
         return Ok(new {gloso = rezulto});
      }
   }
}