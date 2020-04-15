using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace KrestiaVortaro {
   internal static class Program {
      private static async Task Main(string[] args) {
         try {
            switch (args[0]) {
               case "renomigi": {
                  var vortaro = await JsonVortaro.Malfermi(args[1]);
                  var novaVortaro = await Agoj.RenomigiVortojn(vortaro, args[2]);
                  await novaVortaro.Konservi(args[3]);
                  break;
               }
               case "aldoni": {
                  var vortaro = await JsonVortaro.Malfermi(args[1]);
                  var novajVortoj =
                     Agoj.AldoniVortojn(vortaro, File.ReadLines(args[2]));
                  vortaro.Vortoj = novajVortoj.ToList();
                  await vortaro.Konservi(args[3]);
                  break;
               }
               case "krei": {
                  var vortaro = new JsonVortaro();
                  var novajVortoj = Agoj.AldoniVortojn(vortaro, File.ReadLines(args[1]));
                  vortaro.Vortoj!.AddRange(novajVortoj);
                  await vortaro.Konservi(args[2]);
                  break;
               }
               case "kategorigi": {
                  var vortaro = await JsonVortaro.Malfermi(args[1]);
                  var eniro = File.ReadLines(args[2]);
                  vortaro.Kategorioj = Agoj.KategorigiVortojn(eniro, vortaro);
                  await vortaro.Konservi(args[3]);
                  break;
               }
               case "listi": {
                  var vortaro = await JsonVortaro.Malfermi(args[1]);
                  vortaro.Listi();
                  break;
               }
               case "bliss1": {
                  var vortaro = await JsonVortaro.Malfermi(args[1]);
                  await File.WriteAllLinesAsync(args[2], Agoj.KreiListiPorKreiBlissimbolojn(vortaro));
                  break;
               }
               case "bliss2": {
                  var vortaro = await JsonVortaro.Malfermi(args[1]);
                  Agoj.AldoniBlissimbolojnAlVortaro(vortaro, File.ReadLines(args[2]));
                  await vortaro.Konservi(args[3]);
                  break;
               }
            }
         }
         catch (InvalidOperationException e) {
            await Console.Error.WriteLineAsync(e.Message);
         }
      }
   }
}