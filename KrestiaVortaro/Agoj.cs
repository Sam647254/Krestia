﻿using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using KrestiaParser;
using KrestiaVortaroBazo;
using Microsoft.FSharp.Core;
using TimeranDesegnilo2;

namespace KrestiaVortaro; 

public static class Agoj {
   public static IEnumerable<string> KonvertiEnTimeranTxt(IEnumerable<string> eniro) {
      return eniro.Select(vico => vico.Split(' ').Select(vorto => {
         var malinflektita = Decompose.decomposeWord(vorto);
         if (malinflektita.IsNone()) {
            throw new InvalidOperationException($"Could not decompose {vorto}");
         }

         var gramatikajLiteroj = malinflektita.Value.steps.Select(ŝtupo => {
            var literoNomo = ŝtupo.ToString();
            return char.ToLowerInvariant(literoNomo[0]) + literoNomo[1..];
         }).ToImmutableList().Add(malinflektita.Value.baseType.ToString()).Reverse();
         if (gramatikajLiteroj.First() == "noun" && gramatikajLiteroj.Count > 1) {
            gramatikajLiteroj = gramatikajLiteroj.RemoveAt(0);
         }

         var silaboj = Phonotactics.divide(
            DictionaryHelper.timeranStem(malinflektita.Value.baseWord.ToLowerInvariant()));

         if (silaboj.IsError) {
            throw new Exception(silaboj.ErrorValue);
         }

         var partoj = malinflektita.Value.baseType.IsName
            ? new List<string> {
               "[",
               string.Join(' ', silaboj.ResultValue),
            }
            : new List<string> {
               string.Join(' ', silaboj.ResultValue),
            };
         if (malinflektita.Value.baseType.IsName) {
            partoj.Add("]");
         }
         else {
            partoj.AddRange(gramatikajLiteroj);
         }

         return partoj;
      }).SelectMany(v => v)).Select(vortoj => string.Join(' ', vortoj));
   }

   public static void DesegniTimeranTxt(IEnumerable<string> vicoj, string elirejo) {
      var svg = new RektangulaSvgDesegnilo(elirejo, 70, 36, 4, 6);
      foreach (var vico in vicoj) {
         var silaboj = vico.Split(' ');
         foreach (var silabo in silaboj) {
            try {
               svg.DesegniFinaĵon(silabo);
            }
            catch (Exception) {
               svg.DesegniSilabon(silabo);
            }
         }

         svg.Fini();
      }
   }

   private static int KontroliVortonKajValencon(ICollection<string> ekzistantajVortoj, string novaVorto,
      IList<string> radikoj) {
      var malinflektitaVorto = Decompose.decomposeWord(novaVorto);
      var ĉuHavasValidajnRadikojn = radikoj.All(ekzistantajVortoj.Contains);
      var ĉuValidaVorto = Phonotactics.divide(novaVorto);
      var malplenigitajVerboj = DictionaryHelper.reduredFormsOf(novaVorto);
      var ĉuValidajMalplenigitajVerboj = malplenigitajVerboj.IsNone() || malplenigitajVerboj.Value.All(m => {
         var decomposeWord = Decompose.decomposeWord(m);
         return FSharpOption<Decompose.DecomposedWord>.get_IsSome(decomposeWord) &&
                decomposeWord.Value.steps.IsEmpty;
      });

      if (!(malinflektitaVorto.IsSome() && malinflektitaVorto.Value.steps.IsEmpty)) {
         throw new InvalidOperationException($"{novaVorto} ne estas baza vorto");
      }

      if (ĉuValidaVorto.IsError) {
         throw new InvalidOperationException($"{novaVorto} ne havas validajn silabojn");
      }

      if (!ĉuHavasValidajnRadikojn) {
         throw new InvalidOperationException(
            $"{novaVorto} ne havas validajn radikojn ({string.Join(',', radikoj)})");
      }

      if (!ĉuValidajMalplenigitajVerboj) {
         throw new InvalidOperationException($"{novaVorto} ne havas validajn malplenigitajn formojn");
      }

      return Decompose.valencyOf(novaVorto);
   }

   public static void Repari(JsonVortaro vortaro) {
      foreach (var vorto in vortaro.Vortoj!) {
         if (vorto.Ujo1?.Contains('^') == false) continue;
         var signifoPartoj = vorto.Ujo1?.Split('^');
         if (signifoPartoj == null) continue;
         vorto.Signifo = signifoPartoj[0];
         if (signifoPartoj.Length > 1) vorto.Ujo1 = signifoPartoj[1];
         if (signifoPartoj.Length > 2) vorto.Ujo2 = signifoPartoj[2];
         if (signifoPartoj.Length > 3) vorto.Ujo3 = signifoPartoj[3];
      }
   }

   public static IEnumerable<string> AlKv(JsonVortaro vortaro) {
      return AlKv(vortaro.Vortoj!);
   }

   public static IEnumerable<string> AlKv(IEnumerable<Vorto> vortaro) {
      foreach (var vorto in vortaro) {
         var vico = new StringBuilder();
         vico.Append(vorto.PlenaVorto);
         vico.Append('|');
         vico.Append(vorto.Signifo);
         foreach (var ujo in new[] { vorto.Ujo1, vorto.Ujo2, vorto.Ujo3 }) {
            if (ujo == null) continue;
            vico.Append('^');
            vico.Append(ujo);
         }

         vico.Append('|');
         vico.Append(vorto.GlosaSignifo);
         vico.Append('|');
         vico.Append(string.Join(',', vorto.Radikoj));
         vico.Append('|');
         vico.Append(vorto.Noto);
         yield return vico.ToString();
      }
   }

   public static IEnumerable<string> AlKg(JsonVortaro vortaro) {
      return AlKg(vortaro.Kategorioj);
   }

   public static IEnumerable<string> AlKg(IEnumerable<VortaraKategorio> kategorioj) {
      foreach (var kategorio in kategorioj) {
         var vico = new StringBuilder();
         vico.Append(kategorio.Nomo);
         vico.Append(':');
         vico.Append(string.Join(',', kategorio.Vortoj));
         if (kategorio.Subkategorioj.Count > 0) {
            vico.Append(',');
            vico.Append(string.Join(',', kategorio.Subkategorioj.Select(k => $"#{k}")));
         }

         yield return vico.ToString();
      }
   }

   public static ImmutableSortedSet<Vorto> KontroliVortojn(IEnumerable<string> kv) {
      var ĉiujPartoj = (from v in kv where v.Length > 0 select v.Split('|')).ToImmutableList();
      var novajVortojGrupoj = ĉiujPartoj.Select(vico => vico[0]).GroupBy(DictionaryHelper.stemOfWord)
         .ToImmutableHashSet();
      var ĉiujVortoj = ĉiujPartoj.Select(vico => vico[0]).ToImmutableHashSet();
      var plurfojeAldonitajVortoj =
         novajVortojGrupoj.Where(grupo => grupo.Count() > 1).Select(g => g.Key).ToImmutableHashSet();
      var eraroj = new List<Exception>();
      if (!plurfojeAldonitajVortoj.IsEmpty) {
         throw new InvalidOperationException($"Vortoj jam ekzistas: {string.Join(", ", plurfojeAldonitajVortoj)}");
      }

      var rezulto = ĉiujPartoj.Select(partoj => {
         if (partoj.Length < 5) {
            eraroj.Add(new InvalidOperationException($"La vico {string.Join('|', partoj)} estas tro mallonga"));
            return null;
         }

         var vorto = partoj[0];

         if (vorto.EndsWith('l') && !vorto.StartsWith('h') && partoj.Length < 7) {
            eraroj.Add(new InvalidOperationException($"La vico de {vorto} ne sufiĉas da argumentojn"));
            return null;
         }

         var signifajPartoj = partoj[1].Split('^');
         var signifo = signifajPartoj[0];
         var gloso = partoj[2];
         var radikoj = partoj[3].Split(',').Where(r => r.Length > 0).ToImmutableList();
         var noto = partoj[4];
         var valenco = KontroliVortonKajValencon(ĉiujVortoj, vorto, radikoj);
         var ujo1 = valenco >= 1 ? signifajPartoj[1] : null;
         var ujo2 = valenco >= 2 ? signifajPartoj[2] : null;
         var ujo3 = valenco == 3 ? signifajPartoj[3] : null;

         var novaVorto = new Vorto(vorto, DictionaryHelper.stemOfWord(vorto),
            radikoj.Select(r => {
               if (ĉiujVortoj.Contains(r)) {
                  return r;
               }

               throw new InvalidOperationException($"La radiko de {vorto}, {r} ne ekzistas");
            }), signifo, gloso, ujo1, ujo2, ujo3, noto);

         if (partoj.Length < 7) return novaVorto;
         var modifeblajVorttipoj = partoj[5].ToCharArray().Select(t => $"{Vorttipoj[t]}*");
         var modifantoInflekcioj = partoj[6].ToCharArray().Select(i => $"{Inflekcioj[i]}*");

         novaVorto.ModifeblajVorttipoj = modifeblajVorttipoj.ToList();
         novaVorto.ModifantoInflekcioj = modifantoInflekcioj.ToList();

         return novaVorto;
      }).Where(v => v != null).Select(v => v!).ToImmutableSortedSet();

      if (eraroj.Count > 0) {
         throw new AggregateException(null, eraroj);
      }

      return rezulto;
   }

   public static ImmutableSortedSet<VortaraKategorio> KontroliKategoriojn(IImmutableSet<Vorto> vortoj,
      IEnumerable<string> kg) {
      var kategorioj = kg.Where(v => v.Length > 0).Select(vico => {
         var partoj = vico.Split(':');
         var nomo = partoj[0];
         var vortojKajSubkategorioj = partoj[1].Split(',').GroupBy(v => v.StartsWith('#'))
            .ToImmutableDictionary(g => g.Key, g => g.ToImmutableHashSet());
         return new VortaraKategorio(
            nomo,
            subkategorioj: vortojKajSubkategorioj.GetValueOrDefault(true, ImmutableHashSet<string>.Empty)
               .Select(k => k.Substring(1)).ToImmutableHashSet(),
            vortoj: vortojKajSubkategorioj[false]
         );
      }).ToImmutableSortedSet();

      var ekzistantajVortoj = vortoj.Select(v => v.PlenaVorto).ToImmutableHashSet();
      var kategoriajNomoj = kategorioj.Select(k => k.Nomo);
      var erarojn = new List<string>();

      foreach (var kategorio in kategorioj) {
         erarojn.AddRange(from vorto in kategorio.Vortoj
            where !ekzistantajVortoj.Contains(vorto)
            select $"{vorto} ne ekzistas");
         erarojn.AddRange(from subkategorio in kategorio.Subkategorioj
            where !kategoriajNomoj.Contains(subkategorio)
            select $"La kategorio {subkategorio} ne ekzistas");
      }

      if (erarojn.Count > 0) {
         throw new InvalidOperationException(string.Join('\n', erarojn));
      }

      return kategorioj;
   }

   private static readonly Dictionary<string, string> NovajFinaĵoj = new Dictionary<string, string>() {
      { "maa", "ma" },
      { "mo", "me" },
      { "mu", "mi" },
      { "naa", "na" },
      { "no", "ne" },
      { "nu", "ni" },
      { "paa", "pa" },
      { "po", "pe" },
      { "pu", "pi" },
      { "taa", "ta" },
      { "to", "te" },
      { "tu", "ti" },
      { "kaa", "ka" },
      { "ko", "ke" },
      { "ku", "ki" },
      { "gro", "gre" },
      { "gru", "gri" },
      { "dro", "dre" },
      { "dru", "dri" },
   };

   public static ImmutableSortedSet<Vorto> ĜisdatigiVortojn(IImmutableSet<Vorto> vortoj) {
      return vortoj.Select(v => {
         var novaVorto = IgiEnNovaBazo(v.PlenaVorto);
         return new Vorto(novaVorto, DictionaryHelper.stemOfWord(novaVorto), v.Radikoj.Select(IgiEnNovaBazo),
            v.Signifo,
            v.GlosaSignifo, v.Ujo1,
            v.Ujo2, v.Ujo3, v.Noto);
      }).ToImmutableSortedSet();
   }

   public static ImmutableSortedSet<VortaraKategorio> ĜistatigiKategoriojn(
      ImmutableSortedSet<VortaraKategorio> kategorioj) {
      return kategorioj
         .Select(k => new VortaraKategorio(k.Nomo, k.Vortoj.Select(IgiEnNovaBazo).ToImmutableHashSet()))
         .ToImmutableSortedSet();
   }

   public static JsonDictionary
      IgiEnNovaVortaro(IEnumerable<Vorto> vortoj, IEnumerable<VortaraKategorio> kategorioj) {
      var groupoj = vortoj.GroupBy(vorto => DictionaryHelper.typeNameOf(vorto.PlenaVorto))
         .ToDictionary(grupo => grupo.Key);
      var substantivoj = groupoj["Class"].Concat(groupoj["Associative class"]).Select(s => new Noun {
         Spelling = s.PlenaVorto,
         Meaning = s.Signifo,
         Gloss = s.GlosaSignifo,
         Roots = s.Radikoj.ToList(),
         Remarks = s.Noto,
      });
      var verboj = groupoj["Verb"].Select(v => new Verb {
         Spelling = v.PlenaVorto,
         Meaning = v.Signifo,
         Gloss = v.GlosaSignifo,
         Roots = v.Radikoj.ToList(),
         Remarks = v.Noto,
         ArgumentRemarks = new List<string?>(new string?[Decompose.valencyOf(v.PlenaVorto)]),
      });
      var modifantoj = groupoj["Modifier"].Select(s => new Modifier {
         Spelling = s.PlenaVorto,
         Meaning = s.Signifo,
         Gloss = s.GlosaSignifo,
         Roots = s.Radikoj.ToList(),
         Remarks = s.Noto,
         AttachmentTypes = s.ModifantoInflekcioj!,
         CanModifyTypes = s.ModifeblajVorttipoj!,
         AttachmentRemarks = new List<string?>()
      });
      var novajKategorioj = kategorioj.Select(k => new Category {
         Name = k.Nomo,
         Words = k.Vortoj.ToList(),
      });
      var specialajVortoj = groupoj["Digit"].Concat(groupoj["Placeholder"]).Select(v => new DictionaryEntry {
         Spelling = v.PlenaVorto,
         Meaning = v.Signifo,
         Gloss = v.GlosaSignifo,
         Roots = v.Radikoj.ToList(),
         Remarks = v.Noto,
      });

      return new JsonDictionary {
         Nouns = substantivoj.ToList(),
         Records = new List<Record>(),
         Verbs = verboj.ToList(),
         Modifiers = modifantoj.ToList(),
         SpecialWords = specialajVortoj.ToList(),
         Categories = novajKategorioj.ToList()
      };
   }

   private static string IgiEnNovaBazo(string v) {
      var novaFinaĵo = NovajFinaĵoj.FirstOrDefault(f => v.EndsWith(f.Key));
      return !novaFinaĵo.Equals(default(KeyValuePair<string, string>))
         ? v.Substring(0, v.Length - novaFinaĵo.Key.Length) + novaFinaĵo.Value
         : v;
   }

   public static Tuple<IImmutableSet<Vorto>, IImmutableSet<VortaraKategorio>> Ĝisdatigi2(IImmutableSet<Vorto> vortoj,
      IEnumerable<VortaraKategorio> kategorioj) {
      var anstataŭaĵoj = TroviAnstataŭaĵojn(vortoj);
      var novajKategorioj = AnstataŭigiEnKategorio(kategorioj, anstataŭaĵoj);
      var novajVortoj = AnstataŭigiEnRadikoj(vortoj, anstataŭaĵoj);
      return new Tuple<IImmutableSet<Vorto>, IImmutableSet<VortaraKategorio>>(novajVortoj.ToImmutableSortedSet(),
         novajKategorioj.ToImmutableSortedSet());
   }

   public static IEnumerable<string> TroviVerbojn(JsonDictionary dictionary) {
      return dictionary.Verbs.Select(verbo => $"{verbo.Spelling}|{verbo.Meaning}|");
   }

   public static void ĜisdatigiVerbojn(NovaVortaraIndekso vortaro, IEnumerable<string> vicoj) {
      foreach (var vico in vicoj) {
         var partoj = vico.Split('|');
         var verbo = (vortaro.Indekso[partoj[0]] as Verb)!;
         var signifo = partoj[1];
         var frazaSignifo = partoj[2];
         List<string?>? argumentajNotoj = null;
         if (partoj.Length >= 4) {
            argumentajNotoj = partoj[3].Split('^').Select(n => string.IsNullOrEmpty(n) ? null : n).ToList();
            if (argumentajNotoj.Count == 0) {
               argumentajNotoj = null;
            }
         }

         string? noto = null;
         if (partoj.Length == 5) {
            noto = partoj[4];
         }

         verbo.Meaning = signifo;
         verbo.TemplateMeaning = frazaSignifo;
         if (argumentajNotoj != null) {
            verbo.ArgumentRemarks = argumentajNotoj;
         }

         verbo.Remarks = noto;
      }
   }

   public static IEnumerable<string> ListiNekategorigitajVertojn(NovaVortaraIndekso vortaro) {
      var ĉiujVortoj = vortaro.Indekso.Keys.ToImmutableHashSet();
      var kategorigitajVortoj = vortaro.Kategorioj.SelectMany(k => k.Words).ToImmutableHashSet();
      return ĉiujVortoj.Except(kategorigitajVortoj);
   }

   public static void AldoniVortojn(IEnumerable<string> eniro, JsonDictionary dictionary) {
      var indekso = new NovaVortaraIndekso(dictionary);
      foreach (var vico in eniro) {
         var partoj = vico.Split('|');
         var vorto = partoj[0];
         var signifo = partoj[1];
         var gloso = partoj[2];
         var radikoj = partoj[3].Split(',');
         var noto = partoj[4];

         // Kontrolu, ke ĉiuj radikoj ekzistas.
         foreach (var radiko in radikoj) {
            if (!indekso.Indekso.ContainsKey(radiko)) {
               throw new ArgumentException($"Nevalida radiko por {vorto}: {radiko}");
            }
         }

         var vortaraTipo = DictionaryHelper.typeCategoryOf(vorto);
         switch (vortaraTipo) {
            case "Noun":
            case "Associative noun": {
               var plenaFormo = partoj[5];
               dictionary.Nouns.Add(new Noun {
                  Spelling = vorto,
                  Gloss = gloso,
                  Remarks = string.IsNullOrEmpty(noto) ? null : noto,
                  FullForm = string.IsNullOrEmpty(plenaFormo) ? null : plenaFormo,
                  Roots = radikoj.ToList(),
                  Meaning = signifo,
               });
               break;
            }
            case "Verb": {
               var plenaFormo = partoj[5];
               var frazaSignifo = partoj[6];
               var argumentoj = partoj[7].Split('^');
               dictionary.Verbs.Add(new Verb {
                  Spelling = vorto,
                  Gloss = gloso,
                  Remarks = string.IsNullOrEmpty(noto) ? null : noto,
                  FullForm = string.IsNullOrEmpty(plenaFormo) ? null : plenaFormo,
                  Roots = radikoj.ToList(),
                  Meaning = signifo,
                  TemplateMeaning = frazaSignifo,
                  ArgumentRemarks = argumentoj.Select(a => a.Length == 0 ? null : a).ToList()
               });
               break;
            }
            case "Modifier": {
               var modifeblajTipoj = partoj[5].Split(',');
               var aldonaĵajTipoj = partoj[6].Split(',');
               var aldonaĵajNotoj = partoj[7].Split('^');
               dictionary.Modifiers.Add(new Modifier {
                  Spelling = vorto,
                  Gloss = gloso,
                  Remarks = string.IsNullOrEmpty(noto) ? null : noto,
                  Roots = radikoj.ToList(),
                  CanModifyTypes = modifeblajTipoj.ToList(),
                  AttachmentTypes = aldonaĵajTipoj.ToList(),
                  AttachmentRemarks = aldonaĵajNotoj.Select(a => a.Length == 0 ? null : a).ToList(),
                  Meaning = signifo
               });
               break;
            }
            default:
               throw new ArgumentException($"Nevalida vortara tipo por {vorto}: {vortaraTipo}");
         }
      }
   }

   private static IEnumerable<VortaraKategorio> AnstataŭigiEnKategorio(IEnumerable<VortaraKategorio> kategorioj,
      IDictionary<string, string> anstataŭaĵoj) {
      return kategorioj.Select(
         k => new VortaraKategorio(k.Nomo,
            k.Vortoj.Select(v => anstataŭaĵoj.ContainsKey(v) ? anstataŭaĵoj[v] : v).ToImmutableHashSet(),
            k.Subkategorioj));
   }

   private static IEnumerable<Vorto> AnstataŭigiEnRadikoj(IEnumerable<Vorto> vortoj,
      IDictionary<string, string> anstataŭaĵoj) {
      return vortoj.Select(v => {
         var novaPlenaVorto = anstataŭaĵoj.ContainsKey(v.PlenaVorto) ? anstataŭaĵoj[v.PlenaVorto] : v.PlenaVorto;
         return new Vorto(novaPlenaVorto, DictionaryHelper.stemOfWord(novaPlenaVorto),
            v.Radikoj.Select(r => anstataŭaĵoj.ContainsKey(r) ? anstataŭaĵoj[r] : r), v.Signifo, v.GlosaSignifo,
            v.Ujo1, v.Ujo2, v.Ujo3, v.Noto, v.Blissimbolo);
      });
   }

   private static readonly IList<string> KlasajFinaĵoj = new List<string> {
      "pi", "pe", "pa", "ti", "te", "ta", "ki", "ke", "ka",
   };

   private static readonly IDictionary<char, char> Vorttipoj = new Dictionary<char, char> {
      { 'N', 'K' },
      { 'n', 'k' },
      { '0', 'M' },
      { '1', 't' },
      { '2', 'T' },
      { '3', 'D' },
      { '4', 'n' },
      { '5', 'O' },
      { '6', 'Y' },
      { '7', 'N' },
      { 'C', 'C' },
   };

   private static readonly IDictionary<char, char> Inflekcioj = new Dictionary<char, char> {
      { 'D', 'D' },
      { 'N', 'S' },
      { 'F', '@' },
      { 'P', 'P' },
      { 'C', '#' },
   };

   private static IDictionary<string, string> TroviAnstataŭaĵojn(IEnumerable<Vorto> vortoj) {
      var anstataŭaĵoj = new Dictionary<string, string>();
      var random = new Random(0);
      foreach (var vorto in vortoj) {
         if (vorto.PlenaVorto.EndsWith('d')) {
            var bazo = vorto.PlenaVorto[..^1];
            var novaFinaĵo = KlasajFinaĵoj[random.Next(0, KlasajFinaĵoj.Count)];
            anstataŭaĵoj.Add(vorto.PlenaVorto, bazo + novaFinaĵo);
         }
         else if (vorto.PlenaVorto.EndsWith('g') || vorto.PlenaVorto.EndsWith('n')) {
            anstataŭaĵoj.Add(vorto.PlenaVorto, string.Concat(vorto.PlenaVorto.AsSpan(0, vorto.PlenaVorto.Length - 1), "s"));
         }
         else if (vorto.PlenaVorto.EndsWith('v')) {
            anstataŭaĵoj.Add(vorto.PlenaVorto, string.Concat(vorto.PlenaVorto.AsSpan(0, vorto.PlenaVorto.Length - 1), "t"));
         }
         else if (vorto.PlenaVorto.EndsWith("sh")) {
            anstataŭaĵoj.Add(vorto.PlenaVorto, string.Concat(vorto.PlenaVorto.AsSpan(0, vorto.PlenaVorto.Length - 2), "t"));
         }
      }

      return anstataŭaĵoj;
   }
}