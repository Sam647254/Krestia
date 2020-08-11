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
            case Sintaksanalizilo2.Modifanto.Tags.Pridiranto: {
               var pridiranto = value as Sintaksanalizilo2.Modifanto.Pridiranto;
               writer.WriteString("tipo", "Pridiranto");
               writer.WritePropertyName("Argumento");
               JsonSerializer.Serialize(writer, pridiranto!.Item, options);
               break;
            }
            case Sintaksanalizilo2.Modifanto.Tags.EcoDe: {
               var ecoDe = value as Sintaksanalizilo2.Modifanto.EcoDe;
               writer.WriteString("tipo", "EcoDe");
               writer.WritePropertyName("Argumento");
               JsonSerializer.Serialize(writer, ecoDe!.Item, options);
               break;
            }
            default:
               throw new NotImplementedException();
         }
         
         writer.WriteEndObject();
      }
   }
}