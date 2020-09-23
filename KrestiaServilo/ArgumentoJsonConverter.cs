using System;
using System.Text.Json;
using System.Text.Json.Serialization;
using KrestiaVortilo;

namespace KrestiaServilo {
    public class ArgumentoJsonConverter : JsonConverter<Sintaksanalizilo2.Argumento> {
        public override Sintaksanalizilo2.Argumento Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options) {
            throw new NotImplementedException();
        }

        public override void Write(Utf8JsonWriter writer, Sintaksanalizilo2.Argumento value, JsonSerializerOptions options) {
            writer.WriteStartObject();
            
            switch (value.Tag) {
                case Sintaksanalizilo2.Argumento.Tags.ArgumentaVorto: {
                    var vorto = (value as Sintaksanalizilo2.Argumento.ArgumentaVorto)!.Item;
                    writer.WriteString("tipo", "ArgumentaVorto");
                    writer.WritePropertyName("vorto");
                    JsonSerializer.Serialize(writer, vorto, options);
                    break;
                }
                case Sintaksanalizilo2.Argumento.Tags.Mine:
                case Sintaksanalizilo2.Argumento.Tags.Ene: {
                    var argumento = (value as dynamic).Item1;
                    var predikato = (value as dynamic).Item2;
                    writer.WriteString("tipo", value.GetType().Name.ToLowerInvariant());
                    writer.WritePropertyName(value.GetType().Name.ToLowerInvariant());
                    JsonSerializer.Serialize(writer, argumento, options);
                    writer.WritePropertyName("predikato");
                    JsonSerializer.Serialize(writer, predikato, options);
                    break;
                }
                case Sintaksanalizilo2.Argumento.Tags.Keni: {
                    var keni = value as Sintaksanalizilo2.Argumento.Keni;
                    writer.WriteString("tipo", "keni");
                    writer.WritePropertyName("keni");
                    JsonSerializer.Serialize(writer, keni!.Item1, options);
                    writer.WritePropertyName("argumento1");
                    JsonSerializer.Serialize(writer, keni!.Item2, options);
                    writer.WritePropertyName("argumento2");
                    JsonSerializer.Serialize(writer, keni!.Item3, options);
                    break;
                }
                case Sintaksanalizilo2.Argumento.Tags.Pini: {
                    var pini = value as Sintaksanalizilo2.Argumento.Pini;
                    writer.WriteString("tipo", "pini");
                    writer.WritePropertyName("pini");
                    JsonSerializer.Serialize(writer, pini!.Item1, options);
                    writer.WritePropertyName("argumento1");
                    JsonSerializer.Serialize(writer, pini!.Item2, options);
                    writer.WritePropertyName("argumento2");
                    JsonSerializer.Serialize(writer, pini!.Item3, options);
                    writer.WritePropertyName("argumento3");
                    JsonSerializer.Serialize(writer, pini!.Item4, options);
                    break;
                }
                case Sintaksanalizilo2.Argumento.Tags.Nombro: {
                    var nombro = value as Sintaksanalizilo2.Argumento.Nombro;
                    writer.WriteString("tipo", "nombro");
                    writer.WriteNumber("nombro", nombro!.Item);
                    break;
                }
            }
            writer.WriteEndObject();
        }
    }
}