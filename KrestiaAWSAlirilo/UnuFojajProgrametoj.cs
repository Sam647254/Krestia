using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace KrestiaAWSAlirilo {
   static class UnuFojajProgrametoj {
      public static async Task AldoniKategorionAlĈiujVorto(AwsAlirilo awsAlirilo,
         IEnumerable<(string, string)> vortoj) {
         await Task.WhenAll(vortoj.Where(v => v.Item2.Length > 0).Select(v =>
            awsAlirilo.RedaktiVorton(v.Item1, "kategorio", v.Item2.Split(',').ToList())));
      }

      public static async Task AlportiĈiujnVortojn(AwsAlirilo awsAlirilo, string dosiero) {
         var vortoj = await awsAlirilo.AlportiĈiujnVortojn();
         await File.WriteAllLinesAsync(dosiero,
            vortoj.Select(v =>
               $"{v.Vorto}|{v.Signifo}|{string.Join(',', v.Kategorioj)}|{string.Join(',', v.Radikoj)}" +
               $"|{v.Noto}"));
      }
   }
}