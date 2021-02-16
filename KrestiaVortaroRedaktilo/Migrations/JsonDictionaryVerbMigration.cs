using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using KrestiaVortaroBazo;

namespace KrestiaVortaroRedaktilo.Migrations {
   internal static class JsonDictionaryVerbMigration {
      public static void FillInNewDefinitions(IDictionary<string, string> newDefinitions,
         IDictionary<string, IEnumerable<string?>> newNotes, NovaVortaraIndekso dictionary) {
         foreach (var (word, definition) in newDefinitions) {
            dictionary.Indekso[word].Signifo = definition;
         }

         foreach (var (word, note) in newNotes) {
            var verb = dictionary.Indekso[word] as Verbo;
            verb!.ArgumentajNotoj = note.ToList();
         }
      }
   }
}