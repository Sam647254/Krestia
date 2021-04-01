using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using KrestiaVortaroBazo;
using Newtonsoft.Json;
using TimeranDesegnilo2;

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
               case "timeran": {
                  var vicoj = Agoj.KonvertiEnTimeranTxt(File.ReadLines(args[1]));
                  var eniro = vicoj as string[] ?? vicoj.ToArray();
                  var svg = new RektangulaSvgDesegnilo(args[2], 70, 36, 4, 6);
                  foreach (var vico in eniro) {
                     var silaboj = vico.Split(' ');
                     foreach (var silabo in silaboj) {
                        try {
                           svg.DesegniFinaĵon(silabo);
                        }
                        catch (Exception) {
                           svg.DesegniSilabon(silabo);
                        }
                     }
                  }
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
               case "nova": {
                  var kv = File.ReadLines(args[1]);
                  var kg = File.ReadLines(args[2]);
                  var vortoj = Agoj.KontroliVortojn(kv);
                  var kategorioj = Agoj.KontroliKategoriojn(vortoj, kg);
                  var novaVortaro = Agoj.IgiEnNovaVortaro(vortoj, kategorioj);
                  var eliro = JsonConvert.SerializeObject(novaVortaro, Formatting.Indented);
                  await File.WriteAllTextAsync(args[3], eliro);
                  break;
               }
               case "verbo1": {
                  var enhavo = await File.ReadAllTextAsync(args[1]);
                  var vortaro = JsonConvert.DeserializeObject<NovaJsonVortaro>(enhavo);
                  var verboj = Agoj.TroviVerbojn(vortaro);
                  await File.WriteAllLinesAsync(args[2], verboj);
                  break;
               }
               case "verbo2": {
                  var enhavo = await File.ReadAllTextAsync(args[1]);
                  var eniro = File.ReadLines(args[2]);
                  var indekso = new NovaVortaraIndekso(enhavo);
                  Agoj.ĜisdatigiVerbojn(indekso, eniro);
                  await File.WriteAllTextAsync(args[1], indekso.IgiEnJson());
                  break;
               }
               case "nekategorigitaj": {
                  var enhavo = await File.ReadAllTextAsync(args[1]);
                  var indekso = new NovaVortaraIndekso(enhavo);
                  var vortoj = Agoj.ListiNekategorigitajVertojn(indekso).Select(v => $"{v}:");
                  await File.WriteAllLinesAsync(args[2], vortoj);
                  break;
               }
               case "aldoni": {
                  var vortaro = JsonConvert.DeserializeObject<NovaJsonVortaro>(await File.ReadAllTextAsync(args[1]))!;
                  var eniro = File.ReadLines(args[2]);
                  Agoj.AldoniVortojn(eniro, vortaro);
                  var novaVortaro = JsonConvert.SerializeObject(vortaro, Formatting.Indented);
                  await File.WriteAllTextAsync(args[1], novaVortaro);
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