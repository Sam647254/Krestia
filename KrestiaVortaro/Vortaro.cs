using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using KrestiaVortilo;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

namespace KrestiaVortaro {
   public class Vortaro {
      private const string VortaroUrl = "https://raw.githubusercontent.com/Sam647254/Krestia/v0.2/vortaro.json";

      public ImmutableDictionary<string, Vorto> Indekso { get; private set; }
      public ImmutableDictionary<int, Vorto> IdIndekso { get; private set; }
      public ImmutableDictionary<string, ImmutableList<Vorto>>? Kategorioj { get; private set; }
      private ImmutableDictionary<string, Vorto> BazoIndekso { get; set; }

      public IOrderedEnumerable<VortoKunSignifo> Vortlisto =>
         Indekso.Values.Select(vorto => new VortoKunSignifo(vorto.PlenaVorto, vorto.Signifo))
            .OrderBy(v => v.Vorto);

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
         };
      }

      public VortoRezulto TroviVortojn(string peto) {
         var kvanto = peto.Split(' ');
         if (kvanto.Length > 1) {
            var malinflektita = kvanto.Select(Malinflektado.tuteMalinflekti).ToList();
            try {
               return GlosaRezulto(malinflektita.ToList());
            }
            catch (InvalidOperationException) { }
         }

         var malinflekajŜtupoj = Malinflektado.tuteMalinflekti(peto);
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
         IReadOnlyCollection<FSharpResult<Malinflektado.MalinflektitaVorto, string>> vortoj) {
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

      public static async Task<Vortaro> KreiVortaron() {
         Console.WriteLine("KreiVortaron");
         var jsonVortaro = await JsonVortaro.Alporti(VortaroUrl);
         return KreiVortaronDe(jsonVortaro);
      }

      public static Vortaro KreiVortaronDe(JsonVortaro jsonVortaro) {
         var indekso = jsonVortaro.Vortoj.ToImmutableDictionary(v => v.PlenaVorto, v => v);
         return new Vortaro {
            Indekso = indekso,
            BazoIndekso = jsonVortaro.Vortoj.ToImmutableDictionary(v => v.BazaVorto, v => v),
            IdIndekso = jsonVortaro.Vortoj.Select((v, i) => (v, i)).ToImmutableDictionary(p => p.i, p => p.v),
            Kategorioj = jsonVortaro.Kategorioj?.ToImmutableDictionary(k => k.Nomo,
               k => k.Vortoj.Select(v => indekso[v]).ToImmutableList())!,
         };
      }

      public struct VortoKunSignifo {
         public string Vorto { get; }
         public string Signifo { get; }

         public VortoKunSignifo(string vorto, string signifo) {
            Vorto = vorto;
            Signifo = signifo;
         }
      }
   }
}