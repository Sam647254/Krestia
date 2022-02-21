using System;
using System.Collections.Generic;

namespace KrestiaVortaro; 

public class WordResponse {
   private List<string>? _categories;
   private readonly string? _remark;
   private readonly List<string>? _roots;

   public string Spelling { get; set; }
      
   public string? Stem { get; set; }

   public List<string>? Roots {
      get => _roots ?? new List<string>();
      init => _roots = value;
   }

   public string? Meaning { get; init; }

   public string? Gloss { get; init; }

   public string? Remark {
      get => _remark ?? "";
      init => _remark = value;
   }

   public List<string>? Categories {
      get => _categories ?? new List<string>();
      set => _categories = value;
   }

   public string? WordType { get; init; }

   public IEnumerable<string>? Syllables { get; init; }

   public IEnumerable<(string, string)> InflectedForms { get; init; }

   public IEnumerable<string?>? Slots { get; init; }
      
   public string? FullMeaning { get; init; }
      
   public string? Syntax { get; init; }
      
   public IEnumerable<string>? CanModifyWordTypes { get; init; }
      
   public IEnumerable<string>? AttachmentInflections { get; init; }

   internal WordResponse(string spelling) {
      Spelling = spelling;
   }
}