using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using KrestiaVortilo;

namespace KrestiaVortaro {
   public static class Agoj {
      [Obsolete("Ne plu necesas ĉi tion; redaktu rekte la KV-dosieron")]
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

      [Obsolete("Ne plu necesas ĉi tion; redaktu rekte la KV-dosieron")]
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
               if (e is IndexOutOfRangeException) {
                  throw new InvalidOperationException(null, e);
               }

               throw;
            }
         });
         return vortaro.Vortoj.ToImmutableSortedSet().Union(novajVicoj);
      }

      public static IEnumerable<string> KonvertiEnTimeranTxt(JsonVortaro jsonVortaro, IEnumerable<string> eniro,
         bool uziBliss = true) {
         var vortaro = Vortaro.KreiVortaronDe(jsonVortaro);
         foreach (var vico in eniro) {
            var vortoj = vico.Split(' ').Select(vorto => {
               var malinflektita = Malinflektado.tuteMalinflekti(Malinflektado.testaVorto(vorto));
               if (malinflektita.IsError) {
                  throw new InvalidOperationException(malinflektita.ErrorValue.Item2);
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
         var malinflektitaVorto = Malinflektado.malinflekti(Malinflektado.testaVorto(novaVorto));
         var ĉuHavasValidajnRadikojn = radikoj.All(ekzistantajVortoj.Contains);
         var ĉuValidaVorto = Malinflektado.dividi(novaVorto, true);
         var malplenigitajVerboj = Malinflektado.malplenigitajFormojDe(novaVorto);
         var ĉuValidajMalplenigitajVerboj = malplenigitajVerboj.IsError || malplenigitajVerboj.ResultValue.All(m => {
            var ŝtupoj = Malinflektado.malinflekti(Malinflektado.testaVorto(m));
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

         return Malinflektado.valencoDeInfinitivo(novaVorto).Value;
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

      public static IEnumerable<string> AlKv(JsonVortaro vortaro) {
         foreach (var vorto in vortaro.Vortoj!) {
            var vico = new StringBuilder();
            vico.Append(vorto.PlenaVorto);
            vico.Append('|');
            vico.Append(vorto.Signifo);
            foreach (var ujo in new[] {vorto.Ujo1, vorto.Ujo2, vorto.Ujo3}) {
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
         foreach (var kategorio in vortaro.Kategorioj) {
            var vico = new StringBuilder();
            vico.Append(kategorio.Nomo);
            vico.Append(':');
            vico.Append(string.Join(',', kategorio.Vortoj!));
            if (kategorio.Subkategorioj?.Count > 0) {
               vico.Append(',');
               vico.Append(string.Join(',', kategorio.Subkategorioj!.Select(k => $"#{k}")));
            }

            yield return vico.ToString();
         }
      }

      public static ImmutableSortedSet<Vorto> KontroliVortojn(IEnumerable<string> kv) {
         var ĉiujPartoj = (from v in kv where v.Length > 0 select v.Split('|')).ToImmutableList();
         var novajVortojGrupoj = ĉiujPartoj.Select(vico => vico[0]).GroupBy(Malinflektado.bazoDe).ToImmutableHashSet();
         var ĉiujVortoj = ĉiujPartoj.Select(vico => vico[0]).ToImmutableHashSet();
         var plurfojeAldonitajVortoj =
            novajVortojGrupoj.Where(grupo => grupo.Count() > 1).Select(g => g.Key).ToImmutableHashSet();
         if (!plurfojeAldonitajVortoj.IsEmpty) {
            throw new InvalidOperationException($"Vortoj jam ekzistas: {string.Join(", ", plurfojeAldonitajVortoj)}");
         }

         return ĉiujPartoj.Select(partoj => {
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
               if (e is IndexOutOfRangeException) {
                  throw new InvalidOperationException(null, e);
               }

               throw;
            }
         }).ToImmutableSortedSet();
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
                  .Select(k => k.Substring((1))).ToImmutableHashSet(),
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
   }
}