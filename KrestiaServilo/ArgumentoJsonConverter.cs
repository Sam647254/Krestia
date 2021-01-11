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
                case Sintaksanalizilo2.Argumento.Tags.ArgumentaNombro: {
                    var nombro = value as Sintaksanalizilo2.Argumento.ArgumentaNombro;
                    writer.WriteString("tipo", "nombro");
                    writer.WriteNumber("nombro", nombro!.Item.Valuo);
                    break;
                }
            }
            writer.WriteEndObject();
        }
    }
}