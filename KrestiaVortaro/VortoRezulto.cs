using System.Collections.Generic;
using static KrestiaVortaro.Vortaro;

namespace KrestiaVortaro; 

public class VortoRezulto {
   public string? DecomposedWord { get; set; }
   public string? Lemma { get; set; }
   public IEnumerable<WordWithMeaning> Results { get; set; }
   public string? Gloss { get; set; }
   public IEnumerable<string>? DecomposeSteps { get; set; }
   public IEnumerable<string>? GlossWords { get; set; }
   public IEnumerable<IEnumerable<string>>? GlossSteps { get; set; }
   public IEnumerable<string>? BaseWords { get; set; }
   public double? NumberResult { get; set; }

   public VortoRezulto() {
      Results = new List<WordWithMeaning>();
   }
}