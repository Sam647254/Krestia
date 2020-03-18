using System.Linq;
using System.Threading.Tasks;
using KrestiaAWSAlirilo;
using KrestiaVortilo;
using Microsoft.AspNetCore.Mvc;
using Microsoft.FSharp.Collections;

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

      [HttpGet("malinflekti/{eniro}")]
      public async Task<ActionResult> Malinflekti(string eniro) {
         var vortoj = eniro.Split(' ');
         var malinflektitaVortoj = Malinflektado.tuteMalinflektiĈiujn(ListModule.OfArray(vortoj));
         if (malinflektitaVortoj.IsError) {
            return UnprocessableEntity(malinflektitaVortoj.ErrorValue);
         }

         var glosoj = await _awsAlirilo.AlportiVortojn(malinflektitaVortoj.ResultValue.Select(v => v.BazaVorto));

         return Ok(malinflektitaVortoj.ResultValue.Zip(glosoj).Select(p => new {
            p.Second.Gloso,
            MalinflektajŜtupoj = p.First.InflekcioŜtupoj.Where(ŝ => ŝ.IsNebazo)
               .Select(ŝ => ((Sintaksanalizilo.MalinflektaŜtupo.Nebazo) ŝ).Item2.ToString())
         }));
      }

      [HttpGet("vortlisto/alfabeta")]
      public async Task<ActionResult> AlfabetaListo() {
         var vortoj = await _awsAlirilo.AlportiĈiujnVortojn(baza: true);
         return Ok(vortoj.OrderBy(v => v.Vorto));
      }
   }
}