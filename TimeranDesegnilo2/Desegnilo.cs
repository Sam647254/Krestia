using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml;

namespace TimeranDesegnilo2 {
   public abstract class Desegnilo {
      protected abstract Dictionary<string, LiteroDesegnilo> LiteroDesegniloj { get; set; }
      protected abstract Dictionary<string, FinaĵoDesegnilo> FinaĵoDesegniloj { get; set; }

      protected delegate string LiteroDesegnilo(int aktualaLarĝeco, int aktualaAlteco);

      protected delegate string FinaĵoDesegnilo();

      protected readonly int Alteco, Larĝeco;
      protected readonly int DufojaAlteco, DuonaLarĝeco;
      protected double X, Y;
      protected int DokumentoLarĝeco;
      protected readonly int Dx;
      protected readonly int Dy;
      protected readonly int Spaco;
      protected readonly List<string> Vojoj = new();

      private readonly string _elirejo;
      private readonly bool _spacoInterSilaboj;

      protected int Spaceto => (int) (Spaco / 2.0);

      public Desegnilo(string elirejo, int alteco, int larĝeco, int dx, int dy, int spaco = 8,
         bool spacoInterSilaboj = false) {
         _elirejo = elirejo;
         Alteco = alteco / 2;
         Larĝeco = larĝeco + Spaceto;
         Dx = dx;
         Dy = dy;
         Spaco = spaco;
         DufojaAlteco = alteco + Spaceto;
         DuonaLarĝeco = larĝeco / 2;
         DokumentoLarĝeco = 0;
         X = 10;
         Y = 10;
         _spacoInterSilaboj = spacoInterSilaboj;
      }

      public void DesegniSilabon(string silabo) {
         var spacoInterSilaboj = _spacoInterSilaboj ? Spaco : Spaceto;
         switch (silabo.Length) {
            case 1:
               AldoniVojon(LiteroDesegniloj[silabo](Larĝeco, DufojaAlteco), X, Y);
               X += Larĝeco + spacoInterSilaboj;
               break;
            case 2 when ĈuVokalo(silabo[0]):
               AldoniVojon(LiteroDesegniloj[silabo.Substring(0, 1)](DuonaLarĝeco, DufojaAlteco), X, Y);
               X += Larĝeco / 2 + Spaceto;
               AldoniVojon(LiteroDesegniloj[silabo.Substring(1, 1)](DuonaLarĝeco, DufojaAlteco), X, Y);
               X += Larĝeco / 2 + spacoInterSilaboj;
               break;
            case 2: {
               AldoniVojon(LiteroDesegniloj[silabo.Substring(0, 1)](Larĝeco, Alteco), X, Y);
               var antaŭY = Y;
               Y += Alteco + Spaceto;
               AldoniVojon(LiteroDesegniloj[silabo.Substring(1, 1)](Larĝeco, Alteco), X, Y);
               X += Larĝeco + spacoInterSilaboj;
               Y = antaŭY;
               break;
            }
            case 3 when silabo[1] == 'r' | silabo[1] == 'l': {
               AldoniVojon(LiteroDesegniloj[silabo.Substring(0, 2)](Larĝeco, Alteco), X, Y);
               var antaŭY = Y;
               Y += Alteco + Spaceto;
               AldoniVojon(LiteroDesegniloj[silabo.Substring(2, 1)](Larĝeco, Alteco), X, Y);
               X += Larĝeco + spacoInterSilaboj;
               Y = antaŭY;
               break;
            }
            case 3: {
               AldoniVojon(LiteroDesegniloj[silabo.Substring(0, 1)](Larĝeco + Spaceto, Alteco), X, Y);
               var antaŭY = Y;
               Y += Alteco + Spaceto;
               AldoniVojon(LiteroDesegniloj[silabo.Substring(1, 1)](DuonaLarĝeco, Alteco), X, Y);
               X += Larĝeco / 2 + Spaceto;
               AldoniVojon(LiteroDesegniloj[silabo.Substring(2, 1)](DuonaLarĝeco, Alteco), X, Y);
               X += Larĝeco / 2 + spacoInterSilaboj;
               Y = antaŭY;
               break;
            }
            case 4: {
               AldoniVojon(LiteroDesegniloj[silabo.Substring(0, 2)](Larĝeco + Spaceto, Alteco), X, Y);
               var antaŭY = Y;
               Y += Alteco + Spaceto;
               AldoniVojon(LiteroDesegniloj[silabo.Substring(2, 1)](DuonaLarĝeco, Alteco), X, Y);
               X += Larĝeco / 2 + Spaceto;
               AldoniVojon(LiteroDesegniloj[silabo.Substring(3, 1)](DuonaLarĝeco, Alteco), X, Y);
               X += Larĝeco / 2 + spacoInterSilaboj;
               Y = antaŭY;
               break;
            }
            default:
               throw new Exception($"Nevalida silabo: {silabo}");
         }
      }

      private void AldoniVojon(string vojo, double x, double y) {
         Vojoj.Add($"M {(int) x} {(int) y} {vojo}");
      }

      public void DesegniFinaĵon(string finaĵo) {
         var antaŭX = X;
         var antaŭY = Y;
         AldoniVojon(FinaĵoDesegniloj[finaĵo](), (int) antaŭX, (int) antaŭY);
      }

      public virtual void Fini() {
         var xmlSkribilo = XmlWriter.Create(_elirejo);
         xmlSkribilo.WriteStartDocument();
         xmlSkribilo.WriteStartElement("svg", "http://www.w3.org/2000/svg");
         xmlSkribilo.WriteAttributeString("width", Math.Max(X + Spaco, DokumentoLarĝeco).ToString(CultureInfo.InvariantCulture));
         xmlSkribilo.WriteAttributeString("height", (DufojaAlteco + Y + 2 * Spaco).ToString(CultureInfo.InvariantCulture));

         foreach (var vojo in Vojoj) {
            xmlSkribilo.WriteStartElement("path");
            xmlSkribilo.WriteAttributeString("d", vojo);
            xmlSkribilo.WriteEndElement();
         }

         xmlSkribilo.WriteEndElement();
         xmlSkribilo.WriteEndDocument();
         xmlSkribilo.Close();
      }

      private static bool ĈuVokalo(char litero) {
         return litero == 'a' | litero == 'e' | litero == 'i' | litero == 'o' | litero == 'u' | litero == 'ɒ';
      }
   }
}