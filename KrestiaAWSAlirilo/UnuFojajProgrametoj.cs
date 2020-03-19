﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using KrestiaVortilo;
using Microsoft.FSharp.Core;
using MoreLinq.Extensions;

namespace KrestiaAWSAlirilo {
   static class UnuFojajProgrametoj {
      public static async Task AldoniKategorionAlĈiujVortoj(AwsAlirilo awsAlirilo,
         IEnumerable<(string, string)> vortoj) {
         await Task.WhenAll(vortoj.Where(v => v.Item2.Length > 0).Select(v =>
            awsAlirilo.RedaktiVorton(v.Item1, "kategorio", v.Item2.Split(',').ToList())));
      }

      public static async Task AlportiĈiujnVortojn(AwsAlirilo awsAlirilo, string dosiero) {
         var vortoj = await awsAlirilo.AlportiĈiujnVortojn();
         await File.WriteAllLinesAsync(dosiero,
            vortoj.Select(v =>
               $"{v.Vorto}|{v.Signifo}|{string.Join(separator: ',', v.Kategorioj ?? new List<string>())}" +
               $"|{string.Join(separator: ',', v.Radikoj ?? new List<string>())}" +
               $"|{v.Noto}"));
      }

      public static async Task AldoniGlosonAlĈiujVortoj(AwsAlirilo awsAlirilo, IEnumerable<(string, string)> vortoj) {
         await Task.WhenAll(vortoj.Where(v => v.Item2.Length > 0).Select(v =>
            awsAlirilo.RedaktiVorton(v.Item1, "gloso", v.Item2)));
      }

      public static async Task KontroliVortaron(AwsAlirilo awsAlirilo) {
         var vortoj = await awsAlirilo.AlportiĈiujnVortojn();
         vortoj.ForEach(v => {
            var malinflektitaVorto = Malinflektado.malinflekti(v.Vorto);
            var ĉuValidaVorto = Malinflektado.dividi(v.Vorto, true);

            if (!(malinflektitaVorto.IsOk && malinflektitaVorto.ResultValue.IsBazo) || ĉuValidaVorto.IsError) {
               Console.WriteLine($"{v.Vorto} estas nevalida vorto");
            }
         });
      }
   }
}