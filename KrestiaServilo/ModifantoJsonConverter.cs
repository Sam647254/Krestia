using System;
using System.Text.Json;
using System.Text.Json.Serialization;
using KrestiaVortilo;

namespace KrestiaServilo {
   public class ModifantoJsonConverter: JsonConverter<Sintaksanalizilo2.Modifanto> {
      public override Sintaksanalizilo2.Modifanto Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options) {
         throw new NotImplementedException();
      }

      public override void Write(Utf8JsonWriter writer, Sintaksanalizilo2.Modifanto value, JsonSerializerOptions options) {
         writer.WriteStartObject();

         switch (value.Tag) {
            case Sintaksanalizilo2.Modifanto.Tags.Pridiranto:
            case Sintaksanalizilo2.Modifanto.Tags.EcoDe: {
               var pridiranto = value as dynamic;
               writer.WriteString("tipo", value.GetType().Name);
               writer.WritePropertyName("Argumento");
               JsonSerializer.Serialize(writer, pridiranto!.Item, options);
               break;
            }
            case Sintaksanalizilo2.Modifanto.Tags.Modifanto1: {
               var modifanto = value as Sintaksanalizilo2.Modifanto.Modifanto1;
               writer.WriteString("tipo", modifanto!.Item1.BazaVorto);
               writer.WritePropertyName("Argumento");
               JsonSerializer.Serialize(writer, modifanto!.Item2, options);
               break;
            }
            case Sintaksanalizilo2.Modifanto.Tags.SimplaModifanto: {
               var modifanto = value as Sintaksanalizilo2.Modifanto.SimplaModifanto;
               writer.WriteString("tipo", modifanto?.Item.BazaVorto);
               JsonSerializer.Serialize(writer, modifanto!.Item, options);
               break;
            }
            case Sintaksanalizilo2.Modifanto.Tags.ModifantoKunFrazo: {
               var modifanto = value as Sintaksanalizilo2.Modifanto.ModifantoKunFrazo;
               writer.WriteString("tipo", modifanto?.Item1.BazaVorto);
               writer.WritePropertyName("Frazo");
               JsonSerializer.Serialize(writer, modifanto!.Item2, options);
               break;
            }
         }
         
         writer.WriteEndObject();
      }
   }
}