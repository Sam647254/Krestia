using System;
using System.IO;
using System.Linq;
using System.Reflection.Metadata;
using System.Threading.Tasks;

namespace KrestiaVortaro {
   internal static class Program {
      private const string helpaTeksto = @"
██  ████ ████████ ████████ ████████ ████████ ██████ ████████
██████   ██    ██ ██       ██           ██     ██   ██    ██
██  ██   ████████ ████           ██     ██     ██   ████████
██  ████ ██  ██   ████████ ████████     ██   ██████ ██    ██

██    ██ ████████ ████████ ████████ ████████ ████████ ████████
██    ██ ██    ██ ██    ██     ██   ██    ██ ██    ██ ██    ██
██    ██ ██    ██ ████████     ██   ████████ ████████ ██    ██
  ████   ████████ ██  ██       ██   ██    ██ ██  ██   ████████

Komandoj:
kontroli <KV> <KG>
timeran <KV> <eniro> <eliro>
";

      private static async Task Main(string[] args) {
         if (args.Length == 0) {
            Console.WriteLine(helpaTeksto);
            return;
         }

         try {
            switch (args[0]) {
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
                  var vicoj = Agoj.KonvertiEnTimeranTxt(File.ReadLines(args[1]));
                  await File.WriteAllLinesAsync(args[2], vicoj);
                  break;
               }
               case "rondo": {
                  TimeranDesegnilo.Program.Bliss(args[1], args[2], int.Parse(args[3]));
                  break;
               }
               case "timeran": {
                  var vicoj = Agoj.KonvertiEnTimeranTxt(File.ReadLines(args[1]));
                  TimeranDesegnilo.Program.Dosiero(args[2], vicoj, int.Parse(args[3]), int.Parse(args[4]),
                     int.Parse(args[5]), int.Parse(args[6]));
                  break;
               }
               case "timeran2": {
                  var eniro = File.ReadLines(args[1]);
                  TimeranDesegnilo.Program.Dosiero(args[2], eniro, int.Parse(args[3]), int.Parse(args[4]),
                     int.Parse(args[5]), int.Parse(args[6]));
                  break;
               }
               case "kontroli": {
                  var kv = File.ReadLines(args[1]);
                  var kg = File.ReadLines(args[2]);
                  var vortoj = Agoj.KontroliVortojn(kv);
                  Agoj.KontroliKategoriojn(vortoj, kg);
                  break;
               }
               case "ĝisdatigi": {
                  var kv = File.ReadLines(args[1]);
                  var kg = File.ReadLines(args[2]);
                  var vortoj = Agoj.KontroliVortojn(kv);
                  var kategorioj = Agoj.KontroliKategoriojn(vortoj, kg);
                  var novajVortoj = Agoj.ĜisdatigiVortojn(vortoj);
                  var novajKategorioj = Agoj.ĜistatigiKategoriojn(kategorioj);
                  var novaKv = Agoj.AlKv(novajVortoj);
                  var novaKg = Agoj.AlKg(novajKategorioj);
                  Task.WaitAll(File.WriteAllLinesAsync(args[3], novaKv), File.WriteAllLinesAsync(args[4], novaKg));
                  break;
               }
               case "ĝisdatigi2": {
                  var kv = File.ReadLines(args[1]);
                  var kg = File.ReadLines(args[2]);
                  var vortoj = Agoj.KontroliVortojn(kv);
                  var kategorioj = Agoj.KontroliKategoriojn(vortoj, kg);
                  var (novajVortoj, novajKategorioj) = Agoj.Ĝisdatigi2(vortoj, kategorioj);
                  Task.WaitAll(File.WriteAllLinesAsync(args[3], Agoj.AlKv(novajVortoj)),
                     File.WriteAllLinesAsync(args[4], Agoj.AlKg(novajKategorioj)));
                  break;
               }
            }
         }
         catch (InvalidOperationException e) {
            await Console.Error.WriteLineAsync(e.Message);
            await Console.Error.WriteAsync(e.StackTrace);
         }
      }
   }
}