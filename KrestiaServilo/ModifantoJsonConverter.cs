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
         writer.WriteString("tipo", value.GetType().Name);
         switch (value.Tag) {
            case Sintaksanalizilo2.Modifanto.Tags.Pridiranto: {
               var pridiranto = value as Sintaksanalizilo2.Modifanto.Pridiranto;
               writer.WritePropertyName("argumento");
               JsonSerializer.Serialize(writer, pridiranto!.Item, options);
               break;
            }
            case Sintaksanalizilo2.Modifanto.Tags.EcoDe: {
               var pridiranto = value as Sintaksanalizilo2.Modifanto.EcoDe;
               writer.WritePropertyName("argumento");
               JsonSerializer.Serialize(writer, pridiranto!.Item, options);
               break;
            }
            case Sintaksanalizilo2.Modifanto.Tags.Mine: {
               var mine = value as Sintaksanalizilo2.Modifanto.Mine;
               writer.WritePropertyName("predikato");
               JsonSerializer.Serialize(writer, mine!.Item, options);
               break;
            }
            case Sintaksanalizilo2.Modifanto.Tags.Ene: {
               var ene = value as Sintaksanalizilo2.Modifanto.Ene;
               writer.WritePropertyName("predikato");
               JsonSerializer.Serialize(writer, ene!.Item, options);
               break;
            }
            case Sintaksanalizilo2.Modifanto.Tags.Keni: {
               var keni = value as Sintaksanalizilo2.Modifanto.Keni;
               writer.WritePropertyName("argumento1");
               JsonSerializer.Serialize(writer, keni!.Item1, options);
               writer.WritePropertyName("argumento2");
               JsonSerializer.Serialize(writer, keni!.Item2, options);
               break;
            }
            case Sintaksanalizilo2.Modifanto.Tags.Pini: {
               var pini = value as Sintaksanalizilo2.Modifanto.Pini;
               writer.WritePropertyName("argumento1");
               JsonSerializer.Serialize(writer, pini!.Item1, options);
               writer.WritePropertyName("argumento2");
               JsonSerializer.Serialize(writer, pini!.Item2, options);
               writer.WritePropertyName("argumento3");
               JsonSerializer.Serialize(writer, pini!.Item3, options);
               break;
            }
            case Sintaksanalizilo2.Modifanto.Tags.ModifantoKunArgumentoj: {
               var modifanto = value as Sintaksanalizilo2.Modifanto.ModifantoKunArgumentoj;
               writer.WriteString("modifanto", modifanto!.Item1.BazaVorto);
               writer.WritePropertyName("argumento");
               JsonSerializer.Serialize(writer, modifanto!.Item2, options);
               break;
            }
            case Sintaksanalizilo2.Modifanto.Tags.ModifantoKunFrazo: {
               var modifanto = value as Sintaksanalizilo2.Modifanto.ModifantoKunFrazo;
               writer.WriteString("modifanto", modifanto!.Item1.BazaVorto);
               writer.WritePropertyName("frazo");
               JsonSerializer.Serialize(writer, modifanto!.Item2, options);
               break;
            }
            case Sintaksanalizilo2.Modifanto.Tags.Nil: {
               break;
            }
            default: {
               throw new NotImplementedException();
            }
         }
         
         writer.WriteEndObject();
      }
   }
}