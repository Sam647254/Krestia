using System;
using System.Text.Json;
using System.Text.Json.Serialization;
using KrestiaVortilo;

namespace KrestiaServilo {
    public class MalinflektaŜtupoJsonConverter : JsonConverter<Sintaksanalizilo.MalinflektaŜtupo> {
        public override Sintaksanalizilo.MalinflektaŜtupo Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options) {
            throw new NotImplementedException();
        }

        public override void Write(Utf8JsonWriter writer, Sintaksanalizilo.MalinflektaŜtupo value, JsonSerializerOptions options) {
            writer.WriteStartObject();
            writer.WriteString("tipo", value.GetType().Name);
            switch (value.Tag) {
                case Sintaksanalizilo.MalinflektaŜtupo.Tags.Bazo: {
                    var bazo = value as Sintaksanalizilo.MalinflektaŜtupo.Bazo;
                    writer.WriteString("vorttipo", bazo!.Item1.ToString());
                    writer.WriteString("inflekcio", bazo!.Item2.ToString());
                    writer.WriteString("bazaVorto", bazo!.BazaVorto);
                    break;
                }
                case Sintaksanalizilo.MalinflektaŜtupo.Tags.Nebazo: {
                    var nebazo = value as Sintaksanalizilo.MalinflektaŜtupo.Nebazo;
                    writer.WriteString("vorttipo", nebazo!.Item1.ToString());
                    writer.WriteString("inflekcio", nebazo!.Item2.ToString());
                    writer.WriteString("restantaVorto", nebazo!.RestantaVorto);
                    break;
                }
            }
            writer.WriteEndObject();
        }
    }
}