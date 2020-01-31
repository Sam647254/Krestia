using System.Threading.Tasks;
using KrestiaAWSAlirilo;
using Microsoft.AspNetCore.Mvc;

namespace KrestiaServilo.Controllers {
   [ApiController]
   [Route("")]
   public class VortoController : ControllerBase {
      private readonly AwsAlirilo _awsAlirilo;

      public VortoController(AwsAlirilo awsAlirilo) {
         _awsAlirilo = awsAlirilo;
      }

      [HttpGet("vorto/{vorto}")]
      public async Task<ActionResult> Get(string vorto) {
         var vortoRespondo = await _awsAlirilo.AlportiVorton(vorto);
         if (vortoRespondo == null) {
            return NotFound();
         }

         return Ok(vortoRespondo);
      }

      [HttpGet("trovi/{peto}")]
      public async Task<ActionResult> Trovi(string peto) {
         var rezulto = await _awsAlirilo.TroviVortojn(peto);
         return Ok(rezulto);
      }
   }
}