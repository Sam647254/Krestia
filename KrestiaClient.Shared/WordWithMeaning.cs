namespace KrestiaClient.Shared;

public readonly struct WordWithMeaning {
   public string Spelling { get; }
   public string Meaning { get; }

   public WordWithMeaning(string spelling, string meaning) {
      Spelling = spelling;
      Meaning = meaning;
   }
}