using System;
using System.Collections.Generic;
using System.IO;
using System.Net.Http;
using System.Threading.Tasks;
using KrestiaVortilo;
using Newtonsoft.Json;

namespace KrestiaVortaro {
   public class JsonVortaro {
      public List<Vorto>? Vortoj { get; set; } = new List<Vorto>();
      public IEnumerable<VortaraKategorio>? Kategorioj { get; set; } = new List<VortaraKategorio>();

      public async Task Konservi(string eliro) {
         await File.WriteAllTextAsync(eliro, JsonConvert.SerializeObject(this));
      }

      public static async Task<JsonVortaro> Malfermi(string eniro) {
         return JsonConvert.DeserializeObject<JsonVortaro>(await File.ReadAllTextAsync(eniro));
      }

      public static async Task<JsonVortaro> Alporti(string url) {
         var respondo = await new HttpClient().GetStringAsync(url);
         return JsonConvert.DeserializeObject<JsonVortaro>(respondo);
      }

      public void Listi() {
         Vortoj?.ForEach(v => {
            Console.WriteLine(
               $"{v.PlenaVorto} - {Sintaksanalizilo.infinitivoNomoDe(v.PlenaVorto).Value} - {v.Signifo}");
         });
      }
   }

   public class VortaraKategorio {
      public int Id { get; set; }
      public string? Nomo { get; set; }
      public IEnumerable<int>? Vortoj { get; set; }
   }
}