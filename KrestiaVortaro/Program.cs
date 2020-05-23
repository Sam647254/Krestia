﻿using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace KrestiaVortaro {
   internal static class Program {
      private static async Task Main(string[] args) {
         try {
            switch (args[0]) {
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
               case "ripari": {
                  var vortaro = await JsonVortaro.Malfermi(args[1]);
                  Agoj.Repari(vortaro);
                  await vortaro.Konservi(args[2]);
                  break;
               }
               case "bliss": {
                  var vortaro = await JsonVortaro.Malfermi(args[1]);
                  var vicoj = Agoj.KonvertiEnTimeranTxt(vortaro, File.ReadLines(args[2]));
                  await File.WriteAllLinesAsync(args[3], vicoj);
                  break;
               }
               case "timeran": {
                  var vortaro = await JsonVortaro.Malfermi(args[1]);
                  var vicoj = Agoj.KonvertiEnTimeranTxt(vortaro, File.ReadLines(args[2]), false);
                  await File.WriteAllLinesAsync(args[3], vicoj);
                  break;
               }
               case "alkv": {
                  var vortaro = await JsonVortaro.Malfermi(args[1]);
                  var vicoj = Agoj.AlKv(vortaro);
                  await File.WriteAllLinesAsync(args[2], vicoj);
                  break;
               }
               case "alkg": {
                  var vortaro = await JsonVortaro.Malfermi(args[1]);
                  var vicoj = Agoj.AlKg(vortaro);
                  await File.WriteAllLinesAsync(args[2], vicoj);
                  break;
               }
               case "kontroli": {
                  var kv = File.ReadLines(args[1]);
                  Agoj.KontroliVortojn(kv);
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