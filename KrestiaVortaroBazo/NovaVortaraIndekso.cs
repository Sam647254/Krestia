using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Newtonsoft.Json;

namespace KrestiaVortaroBazo {
   public class NovaVortaraIndekso {
      public IImmutableDictionary<string, DictionaryEntry> Indekso { get; private set; } = null!;
      public IEnumerable<Category> Kategorioj => _dictionary.Categories;

      private readonly JsonDictionary _dictionary;

      public NovaVortaraIndekso(string eniro) {
         _dictionary = JsonConvert.DeserializeObject<JsonDictionary>(eniro);
         KreiIndekson();
      }

      public NovaVortaraIndekso(JsonDictionary dictionary) {
         _dictionary = dictionary;
         KreiIndekson();
      }

      public string IgiEnJson() {
         return JsonConvert.SerializeObject(_dictionary, Formatting.Indented);
      }

      private void KreiIndekson() {
         Indekso = _dictionary.Nouns
            .Concat<DictionaryEntry>(_dictionary.Verbs)
            .Concat(_dictionary.Records)
            .Concat(_dictionary.Modifiers)
            .Concat(_dictionary.SpecialWords)
            .ToImmutableDictionary(v => v.Spelling, v => v);
      }
   }
}