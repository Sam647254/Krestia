using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace KrestiaVortaro {
   internal static class Program {
      static async Task Main(string[] args) {
         var vortaro = await JsonVortaro.Malfermi(args[0]);
         switch (args[1]) {
            case "renomigi": {
               var novaVortaro = await Agoj.RenomigiVortojn(vortaro, args[2]);
               await novaVortaro.Konservi(args[3]);
               break;
            }
            case "aldoni": {
               var kategorioj = vortaro.Kategorioj.ToDictionary(k => k.Nomo!, k => k.Vortoj.ToList());
               var novajVortoj =
                  Agoj.AldoniVortojn(
                     vortaro.Vortoj.Select((v, i) => (v, i)).ToDictionary(p => p.v.PlenaVorto, p => p.i), args[2],
                     kategorioj);
               vortaro.Vortoj.AddRange(novajVortoj);
               vortaro.Kategorioj = kategorioj.Select((p, i) => new VortaraKategorio {
                  Id = i,
                  Nomo = p.Key,
                  Vortoj = p.Value
               });
               await vortaro.Konservi(args[3]);
               break;
            }
         }
      }
   }
}