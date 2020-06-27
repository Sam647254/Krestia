using KrestiaVortilo;
using Microsoft.AspNetCore.Mvc;

namespace KrestiaServilo.Controllers {
   [ApiController]
   [Route("api")]
   public class LegiloController : ControllerBase {
      [HttpPost("legi")]
      public IActionResult Legi([FromBody] string eniro) {
         var rezulto = Sintaksanalizilo2.analizi(eniro, false);
         if (rezulto.IsOk) {
            return Ok(rezulto.ResultValue);
         }

         return UnprocessableEntity(rezulto.ErrorValue);
      }
   }
}