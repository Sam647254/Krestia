using System.Collections.Generic;
using System.IO;
using System.Net.Http;
using System.Threading.Tasks;
using Newtonsoft.Json;

namespace KrestiaVortaro {
   public class JsonVortaro {
      public IList<Vorto>? Vortoj { get; set; }
      public IEnumerable<VortaraKategorio>? Kategorioj { get; set; }

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
   }

   public class VortaraKategorio {
      public int Id { get; set; }
      public string Nomo { get; set; }
      public IEnumerable<int> Vortoj { get; set; }
   }
}