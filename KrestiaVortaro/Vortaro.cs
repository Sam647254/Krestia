using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using KrestiaParser;
using KrestiaVortaroBazo;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using static KrestiaParser.DictionaryHelper;

namespace KrestiaVortaro; 

public class Vortaro {
   private readonly IImmutableDictionary<char, int> _alfabeto = "pbmvtdnsʃlrjkgwhieauoɒ".Select((l, i) => (l, i))
      .ToImmutableDictionary(p => p.l, p => p.i);

   private ImmutableDictionary<string, DictionaryEntry> Index { get; }
   private ImmutableDictionary<string, Category>? Categories { get; }
   private ImmutableDictionary<string, DictionaryEntry> StemIndex { get; }

   public IOrderedEnumerable<WordWithMeaning> WordList =>
      Index.Values.Select(vorto => new WordWithMeaning(vorto.Spelling, vorto.Meaning))
         .OrderBy(v => v.Spelling);

   public IImmutableDictionary<string, IOrderedEnumerable<WordWithMeaning>> WordListByType =>
      Index.Values.Select(v => new WordWithMeaning(v.Spelling, v.Meaning)).GroupBy(v =>
            typeNameOf(v.Spelling))
         .ToImmutableSortedDictionary(g => g.Key, g => g.OrderBy(v => v.Spelling));

   public IImmutableDictionary<string, KategorioRespondo> KategoriaVortlisto =>
      Categories!.Select(p => (p.Key,
            new KategorioRespondo(p.Value.Words.Select(v => new WordWithMeaning(v, Index[v].Meaning)))))
         .ToImmutableSortedDictionary(p => p.Key, p => p.Item2);

   private Vortaro(IImmutableSet<DictionaryEntry> vortoj, IImmutableSet<Category> kategorioj) {
      Index = vortoj.ToImmutableDictionary(v => v.Spelling, v => v);
      StemIndex = vortoj.ToImmutableDictionary(v => stemOfWord(v.Spelling), v => v);
      Categories = kategorioj.ToImmutableDictionary(k => k.Name, k => k);
   }

   public VortoRespondo? Vorto(string vorto) {
      var entry = Index.ContainsKey(vorto) ? Index[vorto] : null;
      if (entry == null) {
         return null;
      }

      var wordType = typeNameOf(vorto);
      var syllables = Phonotactics.divide(vorto);

      string? syntax = null;

      if (entry is Modifier modifier) {
         var sb = new StringBuilder(entry.Spelling.Length);
         sb.Append($"&lt;v&gt; {modifier.Spelling}");
         sb.AppendJoin("", modifier.AttachmentTypes.Select((_, i) => $" &lt;x<sub>{i}</sub>&gt;"));
         syntax = sb.ToString();
      }

      if (syllables.IsError) {
         throw new Exception(syllables.ErrorValue);
      }

      var inflekcioj = inflectedFormsOf(vorto);
      return new VortoRespondo(entry.Spelling) {
         Noto = entry.Remarks != null
            ? string.Format(entry.Remarks, "a<sub>1</sub>", "a<sub>2</sub>", "a<sub>3</sub>")
            : null,
         Radikoj = entry.Roots.ToList(),
         Signifo = entry.Meaning,
         Vorttipo = wordType,
         Silaboj = syllables.ResultValue,
         Gloso = entry.Gloss,
         InflektitajFormoj = FSharpOption<FSharpMap<Vorttipo.Inflekcio, string>>.get_IsSome(inflekcioj)
            ? inflekcioj.Value.Select(p => (p.Key.ToString(), p.Value))
               .ToDictionary(p => p.Item1, p => p.Value)
            : null,
         Ujoj = entry is Verb verbo ? verbo.ArgumentRemarks : null,
         FrazaSignifo = entry is Verb verbo2
            ? string.Format(verbo2.TemplateMeaning, "a<sub>1</sub>", "a<sub>2</sub>", "a<sub>3</sub>")
            : null,
         Sintakso = syntax,
         ModifeblajVorttipoj = entry is Modifier m ? m.CanModifyTypes.Select(PriskribiVorttipanMallongaĵon) : null,
         AldonaĵajInflekcioj = entry is Modifier m2 ? m2.AttachmentTypes.Select(PriskribiVorttipanMallongaĵon) : null,
      };
   }

   public VortoRezulto TroviVortojn(string peto) {
      var kvanto = Sintaksanalizilo2.iĝiEnEnirajVortoj(false, peto);
      VortoRezulto? glosaRezulto = null;
      double? nombraRezulto = null;
      if (kvanto.Length > 1) {
         var nombro = Imperativa.proveLegiNombron(peto);
         if (nombro.IsOk) {
            var rezulto =
               Imperativa.kalkuli(Sintaksanalizilo2.Argumento.NewArgumentaNombro(nombro.ResultValue));
            if (rezulto.IsOk) {
               nombraRezulto = rezulto.ResultValue;
            }
         }

         var malinflektita = kvanto.Select(Malinflektado.tuteMalinflekti).ToList();
         try {
            glosaRezulto = GlosaRezulto(malinflektita.ToList());
         }
         catch (InvalidOperationException) { }
      }

      var malinflekajŜtupoj = Malinflektado.tuteMalinflekti(Malinflektado.testaVorto(peto));
      string? malinflektitaVorto = null;
      Vorttipo.Vorttipo? malinflektitaTipo = null;
      string? bazo = null;
      string? bazoGloso = null;
      if (malinflekajŜtupoj.IsOk) {
         var lastaŜtupo = malinflekajŜtupoj.ResultValue.InflekcioŜtupoj.Last()
            as Sintaksanalizilo.MalinflektaŜtupo.Bazo;
         malinflektitaVorto = lastaŜtupo?.BazaVorto;
         malinflektitaTipo = lastaŜtupo?.Item1;
         bazo = Malinflektado.bazoDe(malinflektitaVorto);
         var malinflektitaRezulto = Index.GetValueOrDefault(malinflektitaVorto, null);
         malinflektitaVorto = malinflektitaRezulto?.Spelling ?? malinflektitaVorto;
      }

      if (bazo != null) {
         var bazaRezulto = StemIndex.GetValueOrDefault(bazo, null);

         if (bazaRezulto != null && (bazaRezulto is Noun || bazaRezulto is Verb)) {
            var ĉuMalplenigita = Malinflektado.ĉuMalplenigita(malinflektitaTipo, bazaRezulto.Spelling);
            if (ĉuMalplenigita) {
               bazo = bazaRezulto.Spelling;
            }

            bazoGloso = bazaRezulto.Gloss;
         }
         else {
            bazo = null;
         }
      }

      var rezultoj = Index.AsParallel().Where(p =>
         p.Key.Contains(peto.ToLowerInvariant()) || p.Value.Meaning.Contains(peto.ToLowerInvariant())).ToList();

      return new VortoRezulto {
         MalinflektitaVorto = malinflektitaVorto == peto || bazo == null ? null : malinflektitaVorto,
         PlenigitaVorto = bazo == malinflektitaVorto ? null : bazo,
         Gloso = bazo != null && malinflekajŜtupoj.IsOk && malinflekajŜtupoj.ResultValue.InflekcioŜtupoj.Length > 0
            ? bazoGloso
            : null,
         GlosajVortoj = glosaRezulto?.GlosajVortoj,
         GlosajŜtupoj = glosaRezulto?.GlosajŜtupoj,
         BazajVortoj = glosaRezulto?.BazajVortoj,
         MalinflektajŜtupoj = bazo != null && malinflekajŜtupoj.IsOk
            ? malinflekajŜtupoj.ResultValue.InflekcioŜtupoj.Where(ŝ => ŝ.IsNebazo)
               .Select(ŝ => ((Sintaksanalizilo.MalinflektaŜtupo.Nebazo) ŝ).Item2.ToString())
            : null,
         NombroRezulto = nombraRezulto,
         Rezultoj = rezultoj.Select(r => new WordWithMeaning(r.Key, r.Value.Meaning))
            .OrderBy(vorto => Rilateco(vorto, peto))
      };
   }

   public string? TroviGlosanSignifon(string vorto) {
      var bazo = Malinflektado.bazoDe(vorto);
      var bazaVorto = StemIndex.GetValueOrDefault(bazo, null);
      var vorttipo =
         (Malinflektado.malinflekti(Malinflektado.testaVorto(vorto)).ResultValue as
            Sintaksanalizilo.MalinflektaŜtupo.Bazo)!.Item1;
      if (bazaVorto != null && Malinflektado.ĉuVerbo(vorto).ResultValue &&
          !Malinflektado.ĉuMalplenigita(vorttipo, bazaVorto.Spelling)) {
         return null;
      }

      return bazaVorto?.Gloss;
   }

   private VortoRezulto GlosaRezulto(
      IReadOnlyCollection<FSharpResult<Malinflektado.MalinflektitaVorto, Tuple<Malinflektado.EniraVorto, string>>>
         vortoj) {
      var bazoj = vortoj.Select(v => v.IsOk ? Malinflektado.bazoDe(v.ResultValue.BazaVorto) : "???");
      var rezultoj = bazoj.Select(b => StemIndex.GetValueOrDefault(b, null)).ToList();

      return new VortoRezulto {
         GlosajVortoj = rezultoj.Select(r => r?.Gloss ?? "(not found)"),
         GlosajŜtupoj = vortoj.Select(v => v.IsOk
            ? v.ResultValue.InflekcioŜtupoj.Where(ŝ => ŝ.IsNebazo)
               .Select(ŝ => ((Sintaksanalizilo.MalinflektaŜtupo.Nebazo) ŝ).Item2.ToString())
            : new List<string>()),
         BazajVortoj = rezultoj.Select(r => r?.Spelling ?? "")
      };
   }

   private static int Rilateco(WordWithMeaning vortoRespondo, string peto) {
      if (peto == vortoRespondo.Spelling) {
         return 0;
      }

      if (vortoRespondo.Spelling.StartsWith(peto)) {
         return 1;
      }

      if (vortoRespondo.Meaning == peto) {
         return 2;
      }

      if (vortoRespondo.Meaning?.StartsWith(peto) == true) {
         return 3;
      }

      if (Regex.IsMatch(vortoRespondo.Meaning ?? "", $"\\b{peto}\\b", RegexOptions.IgnoreCase)) {
         return 4;
      }

      return int.MaxValue;
   }

   public static async Task<Vortaro> KreiVortaronDe(string vortaroUrl) {
      var httpClient = new HttpClient();
      var respondo = await httpClient.GetStringAsync(vortaroUrl);
      var indekso = new NovaVortaraIndekso(respondo!);
      return new Vortaro(indekso.Indekso.Values.ToImmutableHashSet(), indekso.Kategorioj.ToImmutableHashSet());
   }

   private static readonly Dictionary<char, string> InflekciajMallongaĵoj = new() {
      {'D', "Definite"},
      {'H', "Possession"},
      {'F', "Focus"},
      {'P', "Progressive"},
      {'p', "Completed"},
      {'I', "Intention"},
      {'d', "Desiderative"},
      {'E', "Predicative identity"},
      {'A', "Attributive identity (Prefix)"},
      {'a', "Attributive identity (postfix)"},
      {'h', "Possessive"},
      {'i', "Imperative"},
      {'1', "Slot 1 argument"},
      {'2', "Slot 2 argument"},
      {'3', "Slot 3 argument"},
      {'e', "Existence"},
      {'t', "Hortative"},
      {'T', "Translative"},
      {'Ĝ', "Gerund"},
      {'ĝ', "Specific gerund"},
      {'U', "Slot 1 filled"},
      {'J', "Slot 2 filled"},
      {'O', "Slot 3 filled"},
      {'S', "Only form"},
      {'s', "Predicative"},
      {'R', "Reflexive"},
      {'4', "Slot 2 first"},
      {'5', "Slot 3 first"},
      {'o', "Optative"},
      {'K', "Quality"},
      {'n', "Hypothetical"},
      {'X', "Detached"},
      {'@', "Name"},
      {'#', "Digit"},
      {'&', "Predicate"}
   };

   private static readonly Dictionary<char, string> VorttipajMallongaĵoj = new() {
      {'K', "Countable class"},
      {'k', "Uncountable class"},
      {'L', "Structural noun (prefix)"},
      {'l', "Structural noun (postfix)"},
      {'E', "Countable associative noun (prefix)"},
      {'e', "Countable associative noun (postfix)"},
      {'P', "Uncountable associative noun (prefix)"},
      {'p', "Uncountable associative noun (postfix)"},
      {'T', "1-2-Verb"},
      {'D', "1-2-3-Verb"},
      {'t', "1-Verb"},
      {'N', "1-3-Verb"},
      {'M', "0-Verb"},
      {'n', "2-Verb"},
      {'O', "2-3-Verb"},
      {'Y', "3-Verb"},
      {'Q', "Placeholder"},
      {'<', "Modifier (postfix)"},
      {'>', "Modifier (prefix)"},
      {'F', "Name"},
      {'C', "Digit"}
   };

   private static string PriskribiVorttipanMallongaĵon(string vorttipo) {
      return vorttipo[1] switch {
         '*' => VorttipajMallongaĵoj[vorttipo[0]],
         '&' => "Predicate",
         _ => vorttipo[0] == '*'
            ? $"Any word under the {InflekciajMallongaĵoj[vorttipo[1]]} inflection"
            : $"{VorttipajMallongaĵoj[vorttipo[0]]} ({InflekciajMallongaĵoj[vorttipo[1]]})"
      };
   }

   public readonly struct WordWithMeaning {
      public string Spelling { get; }
      public string Meaning { get; }

      public WordWithMeaning(string spelling, string meaning) {
         Spelling = spelling;
         Meaning = meaning;
      }
   }

   public class KategorioRespondo {
      public IImmutableList<WordWithMeaning> Vortoj { get; }

      public KategorioRespondo(IEnumerable<WordWithMeaning> vortoj) {
         Vortoj = vortoj.ToImmutableList();
      }
   }
}