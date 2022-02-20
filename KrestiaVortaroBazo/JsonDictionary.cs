using System.Collections.Generic;

namespace KrestiaVortaroBazo; 

public class JsonDictionary {
   public List<Noun> Nouns { get; set; } = null!;
   public List<Record> Records { get; set; } = null!;
   public List<Verb> Verbs { get; set; } = null!;
   public List<Modifier> Modifiers { get; set; } = null!;
   public List<DictionaryEntry> SpecialWords { get; set; } = null!;
   public List<Category> Categories { get; set; } = null!;
}

public class DictionaryEntry {
   public string Spelling { get; set; } = null!;
   public string Meaning { get; set; } = null!;
   public string Gloss { get; set; } = null!;
   public List<string> Roots { get; set; } = null!;
   public string? Remarks { get; set; }
}

public class Noun : DictionaryEntry {
   public string? FullForm { get; set; }
}

public class Record : DictionaryEntry {
   public List<string> ValueTypes { get; } = null!;
   public List<string> ValueMeanings { get; } = null!;
}

public class Verb : DictionaryEntry {
   public List<string?> ArgumentRemarks { get; set; } = null!;
   public string TemplateMeaning { get; set; } = "";
   public string? FullMeaning { get; set; }
}

public class Modifier : DictionaryEntry {
   public List<string> CanModifyTypes { get; set; } = null!;
   public List<string> AttachmentTypes { get; set; } = null!;
   public List<string?> AttachmentRemarks { get; set; } = null!;
}

public class Category {
   public string Name { get; set; } = null!;
   public List<string> Words { get; set; } = null!;
}