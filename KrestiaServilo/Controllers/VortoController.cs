﻿using KrestiaServilo.Services;
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
   }
}