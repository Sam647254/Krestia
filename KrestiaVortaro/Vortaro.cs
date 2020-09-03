using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using KrestiaVortilo;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

namespace KrestiaVortaro {
   public class Vortaro {
      private const string VortaroUrl = "https://raw.githubusercontent.com/Sam647254/Krestia/v0.2/vortaro.json";

      private readonly IImmutableDictionary<char, int> _alfabeto = "pbmvtdnsʃlrjkgwhieauoɒ".Select((l, i) => (l, i))
         .ToImmutableDictionary(p => p.l, p => p.i);

      private ImmutableDictionary<string, Vorto> Indekso { get; }
      private ImmutableDictionary<string, Kategorio>? Kategorioj { get; }
      internal ImmutableDictionary<string, Vorto> BazoIndekso { get; }

      public IOrderedEnumerable<VortoKunSignifo> Vortlisto =>
         Indekso.Values.Select(vorto => new VortoKunSignifo(vorto.PlenaVorto, vorto.Signifo))
            .OrderBy(v => v.Vorto);

      public IImmutableDictionary<string, IOrderedEnumerable<VortoKunSignifo>> TipaVortlisto =>
         Indekso.Values.Select(v => new VortoKunSignifo(v.PlenaVorto, v.Signifo)).GroupBy(v =>
               Malinflektado.vortaraTipoDe(v.Vorto))
            .ToImmutableSortedDictionary(g => g.Key, g => g.OrderBy(v => v.Vorto));

      public IImmutableDictionary<string, KategorioRespondo> KategoriaVortlisto =>
         Kategorioj.Select(p => (p.Key, p.Value.Respondigi())).ToImmutableSortedDictionary(p => p.Key, p => p.Item2);

      private Vortaro(IImmutableSet<Vorto> vortoj, IImmutableSet<VortaraKategorio> kategorioj) {
         Indekso = vortoj.ToImmutableDictionary(v => v.PlenaVorto, v => v);
         BazoIndekso = vortoj.ToImmutableDictionary(v => v.BazaVorto, v => v);
         Kategorioj = kategorioj.ToImmutableDictionary(k => k.Nomo,
            k => new Kategorio {
               Vortoj = k.Vortoj.Select(v => Indekso[v]).ToImmutableList(),
               Subkategorioj = k.Subkategorioj!.ToImmutableHashSet(),
               Superkategorioj = kategorioj.Where(sk => sk.Subkategorioj!.Contains(k.Nomo))
                  .Select(sk => sk.Nomo).ToImmutableHashSet(),
            });
      }

      public VortoRespondo? Vorto(string vorto) {
         var respondo = Indekso.GetValueOrDefault(vorto, null);
         if (respondo == null) {
            return null;
         }

         var vorttipo = Sintaksanalizilo.infinitivoNomoDe(vorto).Value;
         var silaboj = Malinflektado.dividiKunFinaĵo(vorto);

         if (silaboj.IsError) {
            throw new Exception(silaboj.ErrorValue);
         }

         var inflekcioj = Malinflektado.ĉiujInflekciojDe(vorto);
         return new VortoRespondo(respondo.PlenaVorto) {
            Noto = respondo.Noto,
            Radikoj = respondo.Radikoj.ToList(),
            Signifo = respondo.Signifo,
            Vorttipo = vorttipo,
            Silaboj = silaboj.ResultValue,
            Blissimbolo = respondo.Blissimbolo,
            InflektitajFormoj = FSharpOption<FSharpMap<Vorttipo.Inflekcio, string>>.get_IsSome(inflekcioj)
               ? inflekcioj.Value.Select(p => (p.Key.ToString(), p.Value))
                  .ToDictionary(p => p.Item1, p => p.Value)
               : null,
            Ujoj = new[] {respondo.Ujo1, respondo.Ujo2, respondo.Ujo3},
         };
      }

      public VortoRezulto TroviVortojn(string peto) {
         var kvanto = Sintaksanalizilo2.iĝiEnEnirajVortoj(false, peto);
         if (kvanto.Length > 1) {
            var nombro = Imperativa.proveLegiNombron(peto);
            if (nombro.IsOk) {
               try {
                  var nombraRezulto = nombro.ResultValue.Value;
                  return new VortoRezulto {
                     NombroRezulto = nombraRezulto
                  };
               }
               catch (NullReferenceException) { }
            }
            var malinflektita = kvanto.Select(Malinflektado.tuteMalinflekti).ToList();
            try {
               return GlosaRezulto(malinflektita.ToList());
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
            var malinflektitaRezulto = Indekso.GetValueOrDefault(malinflektitaVorto, null);
            malinflektitaVorto = malinflektitaRezulto?.PlenaVorto ?? malinflektitaVorto;
         }

         if (bazo != null) {
            var bazaRezulto = BazoIndekso.GetValueOrDefault(bazo, null);

            if (bazaRezulto != null) {
               var ĉuMalplenigita = Malinflektado.ĉuMalplenigita(malinflektitaTipo, bazaRezulto.PlenaVorto);
               bazo = ĉuMalplenigita ? bazaRezulto.PlenaVorto : bazo;
               bazoGloso = bazaRezulto.GlosaSignifo;
            }
            else {
               bazo = null;
            }
         }

         var rezultoj = Indekso.AsParallel().Where(p =>
            p.Key.Contains(peto.ToLowerInvariant()) || p.Value.Signifo.Contains(peto.ToLowerInvariant())).ToList();

         return new VortoRezulto {
            MalinflektitaVorto = malinflektitaVorto == peto || bazo == null ? null : malinflektitaVorto,
            PlenigitaVorto = bazo == malinflektitaVorto ? null : bazo,
            Gloso = bazo != null && malinflekajŜtupoj.IsOk && malinflekajŜtupoj.ResultValue.InflekcioŜtupoj.Length > 0
               ? bazoGloso
               : null,
            MalinflektajŜtupoj = bazo != null && malinflekajŜtupoj.IsOk
               ? malinflekajŜtupoj.ResultValue.InflekcioŜtupoj.Where(ŝ => ŝ.IsNebazo)
                  .Select(ŝ => ((Sintaksanalizilo.MalinflektaŜtupo.Nebazo) ŝ).Item2.ToString())
               : null,
            Rezultoj = rezultoj.Select(r => new VortoKunSignifo(r.Key, r.Value.Signifo))
               .OrderBy(vorto => Rilateco(vorto, peto))
         };
      }

      private VortoRezulto GlosaRezulto(
         IReadOnlyCollection<FSharpResult<Malinflektado.MalinflektitaVorto, Tuple<Malinflektado.EniraVorto, string>>>
            vortoj) {
         var bazoj = vortoj.Select(v => v.IsOk ? Malinflektado.bazoDe(v.ResultValue.BazaVorto) : "???");
         var rezultoj = bazoj.Select(b => BazoIndekso.GetValueOrDefault(b, null)).ToList();

         return new VortoRezulto {
            GlosajVortoj = rezultoj.Select(r => r?.GlosaSignifo ?? "(not found)"),
            GlosajŜtupoj = vortoj.Select(v => v.IsOk
               ? v.ResultValue.InflekcioŜtupoj.Where(ŝ => ŝ.IsNebazo)
                  .Select(ŝ => ((Sintaksanalizilo.MalinflektaŜtupo.Nebazo) ŝ).Item2.ToString())
               : new List<string>()),
            BazajVortoj = rezultoj.Select(r => r?.PlenaVorto ?? "")
         };
      }

      private static int Rilateco(VortoKunSignifo vortoRespondo, string peto) {
         if (peto == vortoRespondo.Vorto) {
            return 0;
         }

         if (vortoRespondo.Vorto.StartsWith(peto)) {
            return 1;
         }

         if (vortoRespondo.Signifo == peto) {
            return 2;
         }

         if (vortoRespondo.Signifo?.StartsWith(peto) == true) {
            return 3;
         }

         if (Regex.IsMatch(vortoRespondo.Signifo ?? "", $"\\b{peto}\\b", RegexOptions.IgnoreCase)) {
            return 4;
         }

         return int.MaxValue;
      }

      public static Vortaro KreiVortaronDe(JsonVortaro jsonVortaro) {
         return new Vortaro(jsonVortaro.Vortoj!.ToImmutableHashSet(), jsonVortaro.Kategorioj.ToImmutableHashSet());
      }

      public static async Task<Vortaro> KreiVortaronDe(string vortojUrl, string kategoriojUrl) {
         var httpClient = new HttpClient();
         var vortoj = Agoj.KontroliVortojn((await httpClient.GetStringAsync(vortojUrl)).Split('\n'));
         var kategorioj =
            Agoj.KontroliKategoriojn(vortoj, (await httpClient.GetStringAsync(kategoriojUrl)).Split('\n'));
         return new Vortaro(vortoj, kategorioj);
      }

      public readonly struct VortoKunSignifo {
         public string Vorto { get; }
         public string Signifo { get; }

         public VortoKunSignifo(string vorto, string signifo) {
            Vorto = vorto;
            Signifo = signifo;
         }
      }

      public class Kategorio {
         public IImmutableList<Vorto> Vortoj { get; set; }
         public IImmutableSet<string> Subkategorioj { get; set; }
         public IImmutableSet<string> Superkategorioj { get; set; }

         public KategorioRespondo Respondigi() {
            return new KategorioRespondo(Vortoj.Select(v => new VortoKunSignifo(v.PlenaVorto, v.Signifo)),
               Subkategorioj, Superkategorioj);
         }
      }

      public class KategorioRespondo {
         public IImmutableList<VortoKunSignifo> Vortoj { get; }
         public ImmutableSortedSet<string> Subkategorioj { get; }
         public ImmutableSortedSet<string> Superkategorioj { get; }

         public KategorioRespondo(IEnumerable<VortoKunSignifo> vortoj, IEnumerable<string> subkategorioj,
            IEnumerable<string> superkategorioj) {
            Vortoj = vortoj.ToImmutableList();
            Subkategorioj = subkategorioj.ToImmutableSortedSet();
            Superkategorioj = superkategorioj.ToImmutableSortedSet();
         }
      }
   }
}