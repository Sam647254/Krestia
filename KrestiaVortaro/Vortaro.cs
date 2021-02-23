using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using KrestiaVortaroBazo;
using KrestiaVortilo;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using Newtonsoft.Json;

namespace KrestiaVortaro {
   public class Vortaro {
      private const string VortaroUrl = "https://raw.githubusercontent.com/Sam647254/Krestia/v0.2/vortaro.json";

      private readonly IImmutableDictionary<char, int> _alfabeto = "pbmvtdnsʃlrjkgwhieauoɒ".Select((l, i) => (l, i))
         .ToImmutableDictionary(p => p.l, p => p.i);

      private ImmutableDictionary<string, VortaraVorto> Indekso { get; }
      private ImmutableDictionary<string, NovaKategorio>? Kategorioj { get; }
      private ImmutableDictionary<string, VortaraVorto> BazoIndekso { get; }

      public IOrderedEnumerable<VortoKunSignifo> Vortlisto =>
         Indekso.Values.Select(vorto => new VortoKunSignifo(vorto.Vorto, vorto.Signifo))
            .OrderBy(v => v.Vorto);

      public IImmutableDictionary<string, IOrderedEnumerable<VortoKunSignifo>> TipaVortlisto =>
         Indekso.Values.Select(v => new VortoKunSignifo(v.Vorto, v.Signifo)).GroupBy(v =>
               Malinflektado.vortaraTipoDe(v.Vorto))
            .ToImmutableSortedDictionary(g => g.Key, g => g.OrderBy(v => v.Vorto));

      public IImmutableDictionary<string, KategorioRespondo> KategoriaVortlisto =>
         Kategorioj.Select(p => (p.Key,
               new KategorioRespondo(p.Value.Vortoj.Select(v => new VortoKunSignifo(v, Indekso[v].Vorto)))))
            .ToImmutableSortedDictionary(p => p.Key, p => p.Item2);

      private Vortaro(IImmutableSet<VortaraVorto> vortoj, IImmutableSet<NovaKategorio> kategorioj) {
         Indekso = vortoj.ToImmutableDictionary(v => v.Vorto, v => v);
         BazoIndekso = vortoj.ToImmutableDictionary(v => Malinflektado.bazoDe(v.Vorto), v => v);
         Kategorioj = kategorioj.ToImmutableDictionary(k => k.Nomo, k => k);
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
         return new VortoRespondo(respondo.Vorto) {
            Noto = respondo.Noto != null
               ? string.Format(respondo.Noto, "a<sub>1</sub>", "a<sub>2</sub>", "a<sub>3</sub>")
               : null,
            Radikoj = respondo.Radikoj.ToList(),
            Signifo = respondo.Signifo,
            Vorttipo = vorttipo,
            Silaboj = silaboj.ResultValue,
            Gloso = respondo.Gloso,
            InflektitajFormoj = FSharpOption<FSharpMap<Vorttipo.Inflekcio, string>>.get_IsSome(inflekcioj)
               ? inflekcioj.Value.Select(p => (p.Key.ToString(), p.Value))
                  .ToDictionary(p => p.Item1, p => p.Value)
               : null,
            Ujoj = respondo is Verbo verbo ? verbo.ArgumentajNotoj : null,
            FrazaSignifo = respondo is Verbo verbo2
               ? string.Format(verbo2.FrazaSignifo, "a<sub>1</sub>", "a<sub>2</sub>", "a<sub>3</sub>")
               : null,
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
            var malinflektitaRezulto = Indekso.GetValueOrDefault(malinflektitaVorto, null);
            malinflektitaVorto = malinflektitaRezulto?.Vorto ?? malinflektitaVorto;
         }

         if (bazo != null) {
            var bazaRezulto = BazoIndekso.GetValueOrDefault(bazo, null);

            if (bazaRezulto != null) {
               var ĉuMalplenigita = Malinflektado.ĉuMalplenigita(malinflektitaTipo, bazaRezulto.Vorto);
               if (ĉuMalplenigita) {
                  bazo = bazaRezulto.Vorto;
                  bazoGloso = bazaRezulto.Gloso;
               }
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
            GlosajVortoj = glosaRezulto?.GlosajVortoj,
            GlosajŜtupoj = glosaRezulto?.GlosajŜtupoj,
            BazajVortoj = glosaRezulto?.BazajVortoj,
            MalinflektajŜtupoj = bazo != null && malinflekajŜtupoj.IsOk
               ? malinflekajŜtupoj.ResultValue.InflekcioŜtupoj.Where(ŝ => ŝ.IsNebazo)
                  .Select(ŝ => ((Sintaksanalizilo.MalinflektaŜtupo.Nebazo) ŝ).Item2.ToString())
               : null,
            NombroRezulto = nombraRezulto,
            Rezultoj = rezultoj.Select(r => new VortoKunSignifo(r.Key, r.Value.Signifo))
               .OrderBy(vorto => Rilateco(vorto, peto))
         };
      }

      public string? TroviGlosanSignifon(string vorto) {
         var bazo = Malinflektado.bazoDe(vorto);
         var bazaVorto = BazoIndekso.GetValueOrDefault(bazo, null);
         var vorttipo =
            (Malinflektado.malinflekti(Malinflektado.testaVorto(vorto)).ResultValue as
               Sintaksanalizilo.MalinflektaŜtupo.Bazo)!.Item1;
         if (bazaVorto != null && Malinflektado.ĉuVerbo(vorto).ResultValue &&
             !Malinflektado.ĉuMalplenigita(vorttipo, bazaVorto.Vorto)) {
            return null;
         }

         return bazaVorto?.Gloso;
      }

      private VortoRezulto GlosaRezulto(
         IReadOnlyCollection<FSharpResult<Malinflektado.MalinflektitaVorto, Tuple<Malinflektado.EniraVorto, string>>>
            vortoj) {
         var bazoj = vortoj.Select(v => v.IsOk ? Malinflektado.bazoDe(v.ResultValue.BazaVorto) : "???");
         var rezultoj = bazoj.Select(b => BazoIndekso.GetValueOrDefault(b, null)).ToList();

         return new VortoRezulto {
            GlosajVortoj = rezultoj.Select(r => r?.Gloso ?? "(not found)"),
            GlosajŜtupoj = vortoj.Select(v => v.IsOk
               ? v.ResultValue.InflekcioŜtupoj.Where(ŝ => ŝ.IsNebazo)
                  .Select(ŝ => ((Sintaksanalizilo.MalinflektaŜtupo.Nebazo) ŝ).Item2.ToString())
               : new List<string>()),
            BazajVortoj = rezultoj.Select(r => r?.Vorto ?? "")
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

      public static async Task<Vortaro> KreiVortaronDe(string vortaroUrl) {
         var httpClient = new HttpClient();
         var respondo = await httpClient.GetStringAsync(vortaroUrl);
         var indekso = new NovaVortaraIndekso(respondo!);
         return new Vortaro(indekso.Indekso.Values.ToImmutableHashSet(), indekso.Kategorioj.ToImmutableHashSet());
      }

      public readonly struct VortoKunSignifo {
         public string Vorto { get; }
         public string Signifo { get; }

         public VortoKunSignifo(string vorto, string signifo) {
            Vorto = vorto;
            Signifo = signifo;
         }
      }

      public class KategorioRespondo {
         public IImmutableList<VortoKunSignifo> Vortoj { get; }

         public KategorioRespondo(IEnumerable<VortoKunSignifo> vortoj) {
            Vortoj = vortoj.ToImmutableList();
         }
      }
   }
}