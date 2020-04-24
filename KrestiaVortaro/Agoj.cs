using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using KrestiaVortilo;

namespace KrestiaVortaro {
   public static class Agoj {
      internal static async Task<JsonVortaro> RenomigiVortojn(JsonVortaro vortaro, string eniro) {
         var dosiero = await File.ReadAllLinesAsync(eniro);
         var ids = vortaro.Vortoj.Select((v, i) => (v, i)).ToDictionary(p => p.v.PlenaVorto, p => p.i);
         var vortoj = vortaro.Vortoj.ToList();
         foreach (var vico in dosiero) {
            var partoj = vico.Split('|');
            var vorto = vortoj[ids[partoj[0]]];
            vorto.PlenaVorto = partoj[1];
            vorto.BazaVorto = Malinflektado.bazoDe(partoj[1]);
         }

         return new JsonVortaro {
            Vortoj = vortoj,
            Kategorioj = vortaro.Kategorioj,
         };
      }

      public static ImmutableSortedSet<Vorto> AldoniVortojn(JsonVortaro vortaro, IEnumerable<string> dosiero) {
         var ĉiujPartoj = (from v in dosiero select v.Split('|')).ToImmutableList();
         var novajVortojGrupoj = ĉiujPartoj.Select(vico => vico[0]).GroupBy(Malinflektado.bazoDe).ToImmutableHashSet();
         var novajVortoj = ĉiujPartoj.Select(vico => vico[0]).ToImmutableHashSet();
         var plurfojeAldonitajVortoj =
            novajVortojGrupoj.Where(grupo => grupo.Count() > 1).Select(g => g.Key).ToImmutableHashSet();
         if (!plurfojeAldonitajVortoj.IsEmpty) {
            throw new InvalidOperationException($"Plurfoje aldonis: {string.Join(", ", plurfojeAldonitajVortoj)}");
         }

         var ekzistantajVortoj = vortaro.Vortoj.Select(v => v.PlenaVorto).ToImmutableHashSet();
         var ekzistantajBazoj = ekzistantajVortoj.Select(Malinflektado.bazoDe).ToImmutableHashSet();
         var novajBazoj = novajVortoj.Select(Malinflektado.bazoDe).ToImmutableHashSet();
         var denoveAldonitajVortoj = ekzistantajBazoj.Intersect(novajBazoj);
         if (denoveAldonitajVortoj.Any()) {
            throw new InvalidOperationException($"La bazo jam ekzistas: {string.Join(", ", denoveAldonitajVortoj)}");
         }

         var ĉiujVortoj = ekzistantajVortoj.Union(novajVortoj);
         var novajVicoj = ĉiujPartoj.Select(partoj => {
            try {
               var vorto = partoj[0];
               var signifajPartoj = partoj[1].Split('^');
               var signifo = signifajPartoj[0];
               var gloso = partoj[2];
               var radikoj = partoj[3].Split(',').Where(r => r.Length > 0).ToImmutableList();
               var noto = partoj[4];
               var valenco = KontroliVortonKajValencon(ĉiujVortoj, vorto, radikoj);
               var ujo1 = valenco >= 1 ? signifajPartoj[1] : null;
               var ujo2 = valenco >= 2 ? signifajPartoj[2] : null;
               var ujo3 = valenco == 3 ? signifajPartoj[3] : null;

               var novaVorto = new Vorto(vorto, Malinflektado.bazoDe(vorto),
                  radikoj.Select(r => {
                     if (ĉiujVortoj.Contains(r)) {
                        return r;
                     }

                     throw new InvalidOperationException($"La radiko de {vorto}, {r} ne ekzistas");
                  }), signifo, gloso, ujo1, ujo2, ujo3, noto);

               return novaVorto;
            }
            catch (Exception e) {
               Console.WriteLine($"La vico {string.Join('|', partoj)} ne estas nevalida");
               Console.WriteLine(e.Message);
               throw;
            }
         });
         return vortaro.Vortoj.ToImmutableSortedSet().Union(novajVicoj);
      }

      public static IEnumerable<VortaraKategorio> KategorigiVortojn(IEnumerable<string> eniro, JsonVortaro vortaro) {
         var novajKategorioj = eniro.Select(e => {
            var partoj = e.Split(':');
            var kategorioNomo = partoj[0];
            var vortoj = partoj[1].Split(',').GroupBy(v => v.StartsWith('^'))
               .ToImmutableDictionary(g => g.Key, g => g.ToImmutableHashSet());
            return new VortaraKategorio {
               Nomo = kategorioNomo,
               Subkategorioj = vortoj.GetValueOrDefault(true, ImmutableHashSet<string>.Empty)
                  .Select(k => k.Substring(1)).ToList(),
               Vortoj = vortoj[false].ToList(),
            };
         }).ToImmutableList();
         var ekzistantajKategorioj = vortaro.Kategorioj.Select(k => k.Nomo).ToImmutableHashSet();
         var ĉiujKategorioj = ekzistantajKategorioj.Union(novajKategorioj.Select(k => k.Nomo))
            .ToDictionary(k => k, k => new VortaraKategorio {
               Nomo = k,
               Subkategorioj = new List<string>(),
               Vortoj = new List<string>(),
            });
         novajKategorioj.ForEach(k => {
            var kategorio = ĉiujKategorioj[k.Nomo];
            kategorio.Subkategorioj?.AddRange(k.Subkategorioj!);
            kategorio.Vortoj?.AddRange(k.Vortoj!);
         });
         foreach (var kategorio in vortaro.Kategorioj) {
            var novaKategorio = ĉiujKategorioj[kategorio.Nomo];
            novaKategorio.Subkategorioj?.AddRange(kategorio.Subkategorioj!);
            novaKategorio.Vortoj?.AddRange(kategorio.Vortoj!);
         }

         var ĉiujVortoj = vortaro.Vortoj.Select(v => v.PlenaVorto).ToImmutableHashSet();
         foreach (var (kategorioNomo, vortaraKategorio) in ĉiujKategorioj) {
            vortaraKategorio.Subkategorioj?.ForEach(sk => {
               if (!ĉiujKategorioj.ContainsKey(sk)) {
                  throw new InvalidOperationException($"La subkategorio {sk} ne ekzistas");
               }
            });
            vortaraKategorio.Vortoj?.ForEach(v => {
               if (!ĉiujVortoj.Contains(v)) {
                  throw new InvalidOperationException($"La vorto {v} en {kategorioNomo} ne ekzistas");
               }
            });
         }

         return ĉiujKategorioj.Values;
      }

      public static IEnumerable<string> KreiListiPorKreiBlissimbolojn(JsonVortaro vortaro) {
         return vortaro.Vortoj!.Where(v => v.Blissimbolo == null).Select(v => $"{v.PlenaVorto}|{v.Signifo}|");
      }

      public static void AldoniBlissimbolojnAlVortaro(JsonVortaro vortaro, IEnumerable<string> eniro) {
         var vortoj = vortaro.Vortoj!.ToDictionary(v => v.PlenaVorto, v => v);
         foreach (var vico in eniro) {
            var partoj = vico.Split('|');
            var vorto = partoj[0];
            var blissIds = partoj[2].Split(',').Select(id => {
               var ĉuEkzistas = int.TryParse(id, out var blissId);
               return ĉuEkzistas ? blissId : (int?) null;
            }).ToImmutableList();
            if (blissIds.All(id => id != null)) {
               vortoj[vorto].Blissimbolo = blissIds.Select(id => (long) id!.Value).ToList();
            }
         }
      }

      public static IEnumerable<string> KonvertiEnTimeranTxt(JsonVortaro jsonVortaro, IEnumerable<string> eniro,
         bool uziBliss = true) {
         var vortaro = Vortaro.KreiVortaronDe(jsonVortaro);
         foreach (var vico in eniro) {
            var vortoj = vico.Split(' ').Select(vorto => {
               var malinflektita = Malinflektado.tuteMalinflekti(vorto);
               if (malinflektita.IsError) {
                  throw new InvalidOperationException(malinflektita.ErrorValue);
               }

               var gramatikajLiteroj = malinflektita.ResultValue.InflekcioŜtupoj.Select(ŝtupo => {
                  var literoNomo = ŝtupo.IsBazo
                     ? ((Sintaksanalizilo.MalinflektaŜtupo.Bazo) ŝtupo).Item1.ToString()
                     : ((Sintaksanalizilo.MalinflektaŜtupo.Nebazo) ŝtupo).Item2.ToString();
                  return char.ToLowerInvariant(literoNomo[0]) + literoNomo.Substring(1);
               }).Reverse().ToImmutableList();
               if (gramatikajLiteroj.First() == "nombrigeblaKlaso" ||
                   gramatikajLiteroj.First() == "nenombrigeblaKlaso") {
                  gramatikajLiteroj = gramatikajLiteroj.RemoveAt(0);
               }

               var vortoEnVortaro = vortaro.BazoIndekso[
                  Malinflektado.normaligiEnVortaranFormon(malinflektita.ResultValue.BazaVorto)];
               var partoj = new List<string> {
                  uziBliss && vortoEnVortaro.Blissimbolo != null
                     ? vortoEnVortaro.Blissimbolo[0].ToString()
                     : string.Join(' ', Malinflektado.dividi(vortoEnVortaro.BazaVorto, false).ResultValue),
               };
               partoj.AddRange(gramatikajLiteroj);
               return partoj;
            }).SelectMany(v => v);
            yield return string.Join(' ', vortoj);
         }
      }

      private static int KontroliVortonKajValencon(ICollection<string> ekzistantajVortoj, string novaVorto,
         IList<string> radikoj) {
         var malinflektitaVorto = Malinflektado.malinflekti(novaVorto);
         var ĉuHavasValidajnRadikojn = radikoj.All(ekzistantajVortoj.Contains);
         var ĉuValidaVorto = Malinflektado.dividi(novaVorto, true);
         var malplenigitajVerboj = Malinflektado.malplenigitajFormojDe(novaVorto);
         var ĉuValidajMalplenigitajVerboj = malplenigitajVerboj.IsError || malplenigitajVerboj.ResultValue.All(m => {
            var ŝtupoj = Malinflektado.malinflekti(m);
            return ŝtupoj.IsOk && ŝtupoj.ResultValue.IsBazo;
         });

         if (!(malinflektitaVorto.IsOk && malinflektitaVorto.ResultValue.IsBazo)) {
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

         return Malinflektado.valencoDe(novaVorto).Value;
      }

      public static void Repari(JsonVortaro vortaro) {
         foreach (var vorto in vortaro.Vortoj!) {
            if (vorto.Ujo1?.Contains("^") == false) continue;
            var signifoPartoj = vorto.Ujo1?.Split('^');
            if (signifoPartoj == null) continue;
            vorto.Signifo = signifoPartoj[0];
            if (signifoPartoj.Length > 1) vorto.Ujo1 = signifoPartoj[1];
            if (signifoPartoj.Length > 2) vorto.Ujo2 = signifoPartoj[2];
            if (signifoPartoj.Length > 3) vorto.Ujo3 = signifoPartoj[3];
         }
      }
   }
}