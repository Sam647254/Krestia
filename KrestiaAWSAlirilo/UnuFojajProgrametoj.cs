using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace KrestiaAWSAlirilo {
   static class UnuFojajProgrametoj {
      public static async Task AldoniKategorionAlĈiujVorto(AwsAlirilo awsAlirilo,
         IEnumerable<(string, string)> vortoj) {
         await Task.WhenAll(vortoj.Select(v =>
            awsAlirilo.RedaktiVorton(v.Item1, "kategorio", v.Item2.Split(',').ToList())));
      }
   }
}