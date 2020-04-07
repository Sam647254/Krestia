using System;
using System.IO;
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
         }
      }
   }
}