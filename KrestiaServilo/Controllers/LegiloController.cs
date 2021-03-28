﻿using KrestiaVortilo;
using Microsoft.AspNetCore.Mvc;

namespace KrestiaServilo.Controllers {
   [ApiController]
   [Route("api")]
   public class LegiloController : ControllerBase {
      [HttpPost("legi")]
      public IActionResult Legi([FromBody] Peto peto) {
         var rezulto = Imperativa.legiImperative(peto.Eniro);
         if (rezulto.IsOk) {
            return Ok(rezulto.ResultValue);
         }

         return UnprocessableEntity(rezulto.ErrorValue);
      }
   }

   public class Peto {
      public string Eniro { get; init; } = null!;
   }
}