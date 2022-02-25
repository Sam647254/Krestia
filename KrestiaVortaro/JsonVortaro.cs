using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Net.Http;
using System.Threading.Tasks;
using KrestiaParser;
using Newtonsoft.Json;

namespace KrestiaVortaro; 

public class JsonVortaro {
   public List<Vorto>? Vortoj { get; } = new();
   public IEnumerable<VortaraKategorio> Kategorioj { get; } = new List<VortaraKategorio>();

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
            $"{v.PlenaVorto} - {DictionaryHelper.typeNameOf(v.PlenaVorto)} - {v.Signifo}");
      });
   }
}

public class VortaraKategorio : IComparable<VortaraKategorio> {
   public string Nomo { get; }
   public IImmutableSet<string> Vortoj { get; }
   public IImmutableSet<string> Subkategorioj { get; }

   public VortaraKategorio(string nomo, IImmutableSet<string>? vortoj = null,
      IImmutableSet<string>? subkategorioj = null) {
      Nomo = nomo;
      Vortoj = vortoj ?? ImmutableHashSet<string>.Empty;
      Subkategorioj = subkategorioj ?? ImmutableHashSet<string>.Empty;
   }

   public int CompareTo(VortaraKategorio? other) {
      return string.Compare(Nomo, other?.Nomo, StringComparison.Ordinal);
   }
}