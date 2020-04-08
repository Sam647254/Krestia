using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using KrestiaVortaro;
using KrestiaVortilo;
using Microsoft.FSharp.Core;
using MoreLinq.Extensions;
using Newtonsoft.Json;

namespace KrestiaAWSAlirilo {
   internal static class UnuFojajProgrametoj {
      public static async Task AldoniKategorionAlĈiujVortoj(AwsAlirilo awsAlirilo,
         IEnumerable<(string, string)> vortoj) {
         await Task.WhenAll(vortoj.Where(v => v.Item2.Length > 0).Select(v =>
            awsAlirilo.RedaktiVorton(v.Item1, "kategorio", v.Item2.Split(',').ToList())));
      }

      public static async Task AlportiĈiujnVortojn(AwsAlirilo awsAlirilo, string dosiero) {
         var vortoj = await awsAlirilo.AlportiĈiujnVortojn();
         await File.WriteAllLinesAsync(dosiero,
            vortoj.Select(v =>
               $"{v.Vorto}|{v.Signifo}|{string.Join(',', v.Kategorioj ?? new List<string>())}" +
               $"|{string.Join(',', v.Radikoj ?? new List<string>())}" +
               $"|{v.Noto}"));
      }

      public static async Task AldoniGlosonAlĈiujVortoj(AwsAlirilo awsAlirilo, IEnumerable<(string, string)> vortoj) {
         await Task.WhenAll(vortoj.Where(v => v.Item2.Length > 0).Select(v =>
            awsAlirilo.RedaktiVorton(v.Item1, "gloso", v.Item2)));
      }

      /// <summary>
      /// Kontroli, ĉu ĉiuj vortoj en la vortaro estas validaj.
      /// Por esti valida, vorto:
      /// - devas esti infinitivo (laŭ malinflekti)
      /// - devas havas radikojn, kiuj ekzistas en la vortaro
      /// - (se ĝi estas verbo) devas havas malplenigitajn formojn, kiuj estas infinitivoj
      /// </summary>
      /// <param name="awsAlirilo"></param>
      /// <returns></returns>
      public static async Task KontroliVortaron(AwsAlirilo awsAlirilo) {
         var vortoj = (await awsAlirilo.AlportiĈiujnVortojn()).ToImmutableList();
         var vortaro = vortoj.Select(v => v.Vorto).ToImmutableHashSet();
         vortoj.ForEach(v => {
            var rezulto = AwsAlirilo.ĈuValidaVortaraVorto(vortaro, v.Vorto, v.Radikoj);
            if (rezulto != null) {
               Console.WriteLine(rezulto);
            }
         });
      }

      public static async Task ReagordiBazojn(AwsAlirilo awsAlirilo) {
         var vortoj = (await awsAlirilo.AlportiĈiujnVortojn()).ToImmutableList();
         var bazoj = vortoj.Select(v => Malinflektado.bazoDe(v.Vorto));
         await Task.WhenAll(vortoj.Zip(bazoj).Select(p => awsAlirilo.RedaktiVorton(p.First.Vorto, "bazo", p.Second)));
      }

      public static async Task KreiVortaronEnJson(AwsAlirilo awsAlirilo, string eliro) {
         var vortoj = (await awsAlirilo.AlportiĈiujnVortojn()).ToImmutableList();
         var kategorioj = vortoj.Select(v => v.Kategorioj).Flatten().Cast<string>().ToImmutableHashSet()
            .ToImmutableList();
         var vortojEnJson = vortoj.Select((v, i) => new Vorto(
            v.Vorto, v.Bazo, v.Radikoj!, v.Signifo!, v.Gloso, v.Noto));
         var kategoriojEnJson = kategorioj.Select((k, i) => new VortaraKategorio {
            Nomo = k,
            Vortoj = vortoj.Where(v => v.Kategorioj!.Contains(k)).Select(v => v.Vorto).ToList(),
         });
         var jsonVortaro = new JsonVortaro {
            Vortoj = vortojEnJson.ToList(),
            Kategorioj = kategoriojEnJson
         };
         var vortaro = JsonConvert.SerializeObject(jsonVortaro);
         await File.WriteAllTextAsync(eliro, vortaro);
      }
   }
}