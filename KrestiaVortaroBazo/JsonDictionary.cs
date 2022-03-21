using System.Collections.Generic;
using Newtonsoft.Json;

namespace KrestiaVortaroBazo; 

public class JsonDictionary {
   [JsonProperty("Substantivoj")]
   public List<Noun> Nouns { get; set; } = null!;
   
   [JsonProperty("Rekordoj")]
   public List<Record> Records { get; set; } = null!;
   
   [JsonProperty("Verboj")]
   public List<Verb> Verbs { get; set; } = null!;
   
   [JsonProperty("Modifantoj")]
   public List<Modifier> Modifiers { get; set; } = null!;
   
   [JsonProperty("SpecialajVortoj")]
   public List<DictionaryEntry> SpecialWords { get; set; } = null!;
   
   [JsonProperty("Kategorioj")]
   public List<Category> Categories { get; set; } = null!;
}

public class DictionaryEntry {
   [JsonProperty("Vorto")]
   public string Spelling { get; set; } = null!;
   
   [JsonProperty("Signifo")]
   public string Meaning { get; set; } = null!;
   
   [JsonProperty("Gloso")]
   public string Gloss { get; set; } = null!;
   
   [JsonProperty("Radikoj")]
   public List<string> Roots { get; set; } = null!;
   
   [JsonProperty("Noto")]
   public string? Remarks { get; set; }
}

public class Noun : DictionaryEntry {
   [JsonProperty("PlenaFormo")]
   public string? FullForm { get; set; }
}

public class Record : DictionaryEntry {
   public List<string> ValueTypes { get; } = null!;
   public List<string> ValueMeanings { get; } = null!;
}

public class Verb : DictionaryEntry {
   [JsonProperty("ArgumentajNotoj")]
   public List<string?> ArgumentRemarks { get; set; } = null!;
   
   [JsonProperty("FrazaSignifo")]
   public string TemplateMeaning { get; set; } = "";
   
   [JsonProperty("PlenaFormo")]
   public string? FullForm { get; set; }
}

public class Modifier : DictionaryEntry {
   [JsonProperty("ModifeblajTipoj")]
   public List<string> CanModifyTypes { get; set; } = null!;
   
   [JsonProperty("AldonaĵajTipoj")]
   public List<string> AttachmentTypes { get; set; } = null!;
   
   [JsonProperty("AldonaĵajNotoj")]
   public List<string?> AttachmentRemarks { get; set; } = null!;
}

public class Category {
   
   [JsonProperty("Nomo")]
   public string Name { get; set; } = null!;
   
   [JsonProperty("Vortoj")]
   public List<string> Words { get; set; } = null!;
}