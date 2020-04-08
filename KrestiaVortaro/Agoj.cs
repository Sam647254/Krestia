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
            var partoj = vico.Split(separator: '|');
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
         var ĉiujPartoj = dosiero.Select(v => v.Split(separator: '|')).ToImmutableList();
         var novajVortoj = ĉiujPartoj.Select(vico => vico[0]).ToImmutableHashSet();
         var ekzistantajVortoj = vortaro.Vortoj.Select(v => v.PlenaVorto).ToImmutableHashSet();
         var denoveAldonitajVortoj = ekzistantajVortoj.Intersect(novajVortoj);
         if (!denoveAldonitajVortoj.IsEmpty) {
            throw new InvalidOperationException($"Jam ekzistas: {denoveAldonitajVortoj}");
         }

         var ĉiujVortoj = ekzistantajVortoj.Union(novajVortoj);
         var novajVicoj = ĉiujPartoj.Select(partoj => {
            if (partoj.Length != 5) {
               throw new InvalidOperationException($"{string.Join(separator: '|', partoj)} ne estas valida");
            }

            var vorto = partoj[0];
            var signifajPartoj = partoj[1].Split(separator: '^');
            var signifo = signifajPartoj[0];
            var gloso = partoj[2];
            var radikoj = partoj[3].Split(separator: ',').Where(r => r.Length > 0).ToImmutableList();
            var noto = partoj[4];
            var valenco = KontroliVortonKajValencon(ĉiujVortoj, vorto, radikoj);
            var ujo1 = valenco >= 1 ? partoj[1] : null;
            var ujo2 = valenco >= 2 ? partoj[2] : null;
            var ujo3 = valenco == 3 ? partoj[3] : null;

            var novaVorto = new Vorto(vorto, Malinflektado.bazoDe(vorto),
               radikoj.Select(r => {
                  if (ĉiujVortoj.Contains(r)) {
                     return r;
                  }

                  throw new InvalidOperationException($"La radiko de {vorto}, {r} ne ekzistas");
               }), signifo, gloso, ujo1, ujo2, ujo3, noto);

            return novaVorto;
         });
         return vortaro.Vortoj.ToImmutableSortedSet().Union(novajVicoj);
      }

      private static int KontroliVortonKajValencon(ICollection<string> ekzistantajVortoj, string novaVorto,
         IList<string> radikoj) {
         var malinflektitaVorto = Malinflektado.malinflekti(novaVorto);
         var ĉuHavasValidajnRadikojn = radikoj.All(ekzistantajVortoj.Contains);
         var ĉuValidaVorto = Malinflektado.dividi(novaVorto, inkluziFinaĵon: true);
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
               $"{novaVorto} ne havas validajn radikojn ({string.Join(separator: ',', radikoj)}))");
         }

         if (!ĉuValidajMalplenigitajVerboj) {
            throw new InvalidOperationException($"{novaVorto} ne havas validajn malplenigitajn formojn");
         }

         return Malinflektado.valencoDe(novaVorto);
      }
   }
}