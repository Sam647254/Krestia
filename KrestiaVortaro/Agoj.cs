using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using KrestiaVortilo;

namespace KrestiaVortaro {
   internal static class Agoj {
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
            Kategorioj = vortaro.Kategorioj
         };
      }

      internal static IEnumerable<Vorto> AldoniVortojn(IDictionary<string, int> ekzistantajVortoj, string eniro,
         IDictionary<string, List<int>> vortarajKategorioj) {
         var komencoId = ekzistantajVortoj.Count;
         var dosiero = File.ReadLines(eniro);
         foreach (var vico in dosiero) {
            var partoj = vico.Split(separator: '|');
            var vorto = partoj[0];
            var signifo = partoj[1];
            var gloso = partoj[2];
            var radikoj = partoj[3].Split(separator: ',').Where(r => r.Length > 0).ToImmutableList();
            var kategorioj = partoj[4].Split(separator: ',');
            var noto = partoj[5];
            var eraro = ĈuValidaVortaraVorto(ekzistantajVortoj, vorto, radikoj);
            if (eraro != null) {
               throw new InvalidOperationException(eraro);
            }

            var novaVorto = new Vorto(komencoId, vorto, Malinflektado.bazoDe(vorto),
               radikoj.Select(r => ekzistantajVortoj[r]), signifo, gloso, noto);

            foreach (var kategorio in kategorioj) {
               if (vortarajKategorioj.ContainsKey(kategorio)) {
                  vortarajKategorioj[kategorio].Add(novaVorto.Id);
               }
               else {
                  vortarajKategorioj.Add(kategorio, new List<int> {novaVorto.Id});
               }
            }

            yield return novaVorto;
         }
      }

      private static string? ĈuValidaVortaraVorto(IDictionary<string, int> ekzistantajVortoj, string novaVorto,
         IList<string> radikoj) {
         var malinflektitaVorto = Malinflektado.malinflekti(novaVorto);
         var ĉuHavasValidajnRadikojn = radikoj.All(ekzistantajVortoj.ContainsKey);
         var ĉuValidaVorto = Malinflektado.dividi(novaVorto, inkluziFinaĵon: true);
         var malplenigitajVerboj = Malinflektado.malplenigitajFormojDe(novaVorto);
         var ĉuValidajMalplenigitajVerboj = malplenigitajVerboj.IsError || malplenigitajVerboj.ResultValue.All(m => {
            var ŝtupoj = Malinflektado.malinflekti(m);
            return ŝtupoj.IsOk && ŝtupoj.ResultValue.IsBazo;
         });

         if (!(malinflektitaVorto.IsOk && malinflektitaVorto.ResultValue.IsBazo)) {
            return $"{novaVorto} ne estas baza vorto";
         }

         if (ĉuValidaVorto.IsError) {
            return $"{novaVorto} ne havas validajn silabojn";
         }

         if (!ĉuHavasValidajnRadikojn) {
            return $"{novaVorto} ne havas validajn radikojn ({string.Join(',', radikoj)})";
         }

         if (!ĉuValidajMalplenigitajVerboj) {
            return $"{novaVorto} ne havas validajn malplenigitajn formojn";
         }

         if (ekzistantajVortoj.ContainsKey(novaVorto)) {
            return $"{novaVorto} jam ekzistas";
         }

         return null;
      }
   }
}