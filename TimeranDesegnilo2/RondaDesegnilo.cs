using System;
using System.Collections.Generic;
using System.Xml.Linq;

namespace TimeranDesegnilo2 {
   public class BlissSvgDesegnilo : Desegnilo {
      private int XMax { get; set; }

      protected sealed override Dictionary<string, LiteroDesegnilo> LiteroDesegniloj { get; set; }

      protected sealed override Dictionary<string, FinaĵoDesegnilo> FinaĵoDesegniloj { get; set; }

      private readonly XDocument _xmlDokumento;
      private readonly string _elirejo;

      private string NomoKomenco() {
         X += DuonaLarĝeco + Spaco;
         return string.Format("m {0} 0 h -{0} v {1} h {0}", DuonaLarĝeco, DufojaAlteco);
      }

      private static string P(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("m 0 {0} v -{0} h {1}", aktualaAlteco, aktualaLarĝeco);
      }

      private static string M(int aktualaLarĝeco, int aktualaAlteco) {
         return $"v {aktualaAlteco} h {aktualaLarĝeco}";
      }

      private static string K(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {1} v {0}", aktualaAlteco, aktualaLarĝeco);
      }

      private string Klaso() {
         X += DuonaLarĝeco + Spaco;
         return string.Format("m {0} 0 l -{0} {1} l {0} {1}", DuonaLarĝeco, DufojaAlteco / 2);
      }

      public BlissSvgDesegnilo(string elirejo, int radio, int spaco = 22) : base(elirejo,
         DufojaAlteco - spaco / 2 - 5,
         Larĝeco, radio, radio, spaco, true) {
         LiteroDesegniloj = new Dictionary<string, LiteroDesegnilo> {
            {"p", P}, {
               "b",
               (aktualaLarĝeco, aktualaAlteco) => string.Format("m 0 {0} v -{0} h {1} m -{2} 0 v {0}", aktualaAlteco,
                  aktualaLarĝeco,
                  aktualaLarĝeco / 2)
            },
            {"m", M}, {
               "v",
               (aktualaLarĝeco, aktualaAlteco) => string.Format("m {0} 0 h -{0} l {1} {2}", aktualaLarĝeco,
                  aktualaLarĝeco >= Larĝeco ? aktualaLarĝeco / 2 : aktualaLarĝeco, aktualaAlteco)
            }, {
               "t",
               (aktualaLarĝeco, aktualaAlteco) => string.Format("h {1} m -{2} 0 v {0}", aktualaAlteco, aktualaLarĝeco,
                  aktualaLarĝeco / 2)
            }, {
               "d",
               (aktualaLarĝeco, aktualaAlteco) => string.Format("h {1} m -{2} 0 v {0} m {3} -{0} v {0}", aktualaAlteco,
                  aktualaLarĝeco,
                  aktualaLarĝeco * 2 / 3, aktualaLarĝeco / 3)
            }, {
               "n",
               (aktualaLarĝeco, aktualaAlteco) => string.Format("m 0 {0} h {1} m -{2} -{0} v {0}", aktualaAlteco,
                  aktualaLarĝeco,
                  aktualaLarĝeco / 2)
            }, {
               "s",
               (aktualaLarĝeco, aktualaAlteco) => string.Format("h {0} m -{0} {1} l {2} -{1} l {2} {1}", aktualaLarĝeco,
                  aktualaAlteco,
                  aktualaLarĝeco / 2)
            }, {
               "l",
               (aktualaLarĝeco, aktualaAlteco) => string.Format("h {0} v {1} h -{0} z", aktualaLarĝeco, aktualaAlteco)
            }, {
               "r",
               (aktualaLarĝeco, aktualaAlteco) => $"h {aktualaLarĝeco} l -{aktualaLarĝeco / 2} {aktualaAlteco} z"
            }, {
               "ʃ",
               (aktualaLarĝeco, aktualaAlteco) =>
                  string.Format("h {0} v {1} m -{0} 0 l {0} -{1}", aktualaLarĝeco, aktualaAlteco)
            },
            {"k", K}, {
               "h",
               (aktualaLarĝeco, aktualaAlteco) => string.Format("h {0} m -{0} {1} h {0}", aktualaLarĝeco, aktualaAlteco)
            }, {
               "tl",
               (aktualaLarĝeco, aktualaAlteco) => string.Format("h {0} m -{1} 0 v {3} h {2} v -{3}", aktualaLarĝeco,
                  aktualaLarĝeco * 2 / 3,
                  aktualaLarĝeco / 3, aktualaAlteco)
            }, {
               "tr",
               (aktualaLarĝeco, aktualaAlteco) => string.Format("h {0} m -{1} 0 l {2} {3} l {2} -{3}", aktualaLarĝeco,
                  aktualaLarĝeco * 2 / 3, aktualaLarĝeco / 6, aktualaAlteco)
            }, {
               "kr",
               (aktualaLarĝeco, aktualaAlteco) => string.Format("m {0} 0 l -{1} {2} l -{1} -{2} h {3} v {2}",
                  aktualaLarĝeco / 2,
                  aktualaLarĝeco / 4, aktualaAlteco, aktualaLarĝeco)
            }, {
               "gr",
               (aktualaLarĝeco, aktualaAlteco) => string.Format(
                  "m {0} 0 l -{1} {2} l -{1} -{2} h {3} v {2} m -{4} -{2} v {2}",
                  aktualaLarĝeco / 2, aktualaLarĝeco / 4, aktualaAlteco, aktualaLarĝeco, aktualaLarĝeco / 3)
            },
            {"i", P}, {
               "e",
               (aktualaLarĝeco, aktualaAlteco) => $"v {aktualaAlteco} m 0 -{aktualaAlteco / 2} h {aktualaLarĝeco}"
            },
            {"a", M}, {"u", K}, {
               "o",
               (aktualaLarĝeco, aktualaAlteco) => string.Format("m 0 {1} h {2} m 0 -{1} v {0}", aktualaAlteco,
                  aktualaAlteco / 2,
                  aktualaLarĝeco)
            }, {
               "ɒ",
               (aktualaLarĝeco, aktualaAlteco) => string.Format("m 0 {0} h {1} v -{0}", aktualaAlteco, aktualaLarĝeco)
            }, {
               "x",
               (aktualaLarĝeco, aktualaAlteco) =>
                  string.Format("m 0 {0} l {1} -{0} l {1} {0}", aktualaAlteco, aktualaLarĝeco / 2)
            }
         };
         FinaĵoDesegniloj = new Dictionary<string, FinaĵoDesegnilo> {
            {"[", NomoKomenco}, {
               "]", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("h {0} v {1} h -{0}", DuonaLarĝeco, DufojaAlteco);
               }
            }, {
               "lokokupilo", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("m 0 {0} h {1} v -{0} m -{2} {0} v -{0}", DufojaAlteco, DuonaLarĝeco,
                     DuonaLarĝeco / 2);
               }
            },
            {"nombrigeblaKlaso", Klaso}, {"nenombrigeblaKlaso", Klaso}, {
               "antaŭNenombrigeblaEco", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("m {0} 0 a {0} {1} 0 0 0 0 {2} m -{0} -{3} h {0}", DuonaLarĝeco,
                     DufojaAlteco / 2, DufojaAlteco, DufojaAlteco / 2);
               }
            }, {
               "malantaŭNenombrigeblaEco", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("a {0} {1} 0 0 1 0 {2} m 0 -{3} h {0}", DuonaLarĝeco, DufojaAlteco / 2,
                     DufojaAlteco, DufojaAlteco / 2);
               }
            }, {
               "antaŭNombrigeblaEco", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("m {0} 0 h -{0} v {1} h {0} m -{0} -{2} h {0}", DuonaLarĝeco, DufojaAlteco,
                     DufojaAlteco / 2);
               }
            }, {
               "rekordo<", () => {
                  X += DuonaLarĝeco + Spaceto + Spaco;
                  return string.Format("a {0} {1} 0 0 1 0 {2} m {3} -{2} v {4} m 0 {5} v {4}", DuonaLarĝeco,
                     DufojaAlteco / 2, DufojaAlteco, DuonaLarĝeco + Spaceto, Alteco, Spaceto);
               }
            }, {
               "rekordo>", () => {
                  X += DuonaLarĝeco + Spaceto + Spaco;
                  return string.Format("m {0} 0 a {0} {1} 0 0 0 0 {2} m {4} -{2} v {3} m 0 {4} v {3}",
                     DuonaLarĝeco, DufojaAlteco / 2, DufojaAlteco, Alteco, Spaceto);
               }
            }, {
               "pridiranto", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("m {0} 0 l -{0} {1} h {2} z", DuonaLarĝeco / 2, DufojaAlteco,
                     DuonaLarĝeco);
               }
            }, {
               "atributivoEstiMalantaŭ", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("m 0 {0} h {1} m -{1} {0} h {1} m 0 -{2} v {3}", DufojaAlteco / 3,
                     DuonaLarĝeco, DufojaAlteco * 2 / 3, DufojaAlteco);
               }
            }, {
               "atributivoEstiAntaŭ", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("v {3} m 0 -{2} h {1} m -{1} {0} h {1}", DufojaAlteco / 3, DuonaLarĝeco,
                     DufojaAlteco * 2 / 3, DufojaAlteco);
               }
            }, {
               "predikativoEsti", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("m 0 {0} h {1} m -{2} -{0} v {3} m {2} -{3} v {3}", DufojaAlteco / 2,
                     DuonaLarĝeco, DuonaLarĝeco / 2, DufojaAlteco);
               }
            }, {
               "netransitivaVerbo", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("m 0 {0} h {1} m -{2} -{0} v {3}", DufojaAlteco / 2, DuonaLarĝeco,
                     DuonaLarĝeco / 2, DufojaAlteco);
               }
            }, {
               "nedirektaTransitivaVerbo", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("h {0} m -{1} 0 v {2} m -{1} 0 h {0}", DuonaLarĝeco, DuonaLarĝeco / 2,
                     DufojaAlteco);
               }
            }, {
               "transitivaVerbo", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("h {0} l -{0} {1} h {0}", DuonaLarĝeco, DufojaAlteco);
               }
            }, {
               "oblikaTransitivaVerbo", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("l {0} {1} l {0} -{1} m -{2} {3} h {2} m -{0} 0 v {1} m -{0} 0 h {2}",
                     DuonaLarĝeco / 2, Alteco, DuonaLarĝeco, Alteco + Spaceto);
               }
            }, {
               "malantaŭModifanto", () => {
                  X += DuonaLarĝeco + Spaco;
                  return $"h {DuonaLarĝeco} v {DufojaAlteco}";
               }
            }, {
               "antaŭModifanto", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("m {0} 0 h -{0} v {1}", DuonaLarĝeco, DufojaAlteco);
               }
            }, {
               "difinito", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("m {0} 0 l -{0} {1} l {0} {1} m 0 -{1} h -{0}", DuonaLarĝeco,
                     DufojaAlteco / 2);
               }
            }, {
               "pluraNombro", () => {
                  X += DuonaLarĝeco + Spaco + Spaceto;
                  return string.Format(
                     "m {0} 0 l -{0} {1} l {0} {1} m -{0} -{1} h {0} m {2} -{1} v {3} m 0 {2} v {3}",
                     DuonaLarĝeco, DufojaAlteco / 2, Spaceto, Alteco);
               }
            }, {
               "havaĵo", () => {
                  X += DuonaLarĝeco;
                  return string.Format("m 0 {0} h {1} m 0 -{0} v {2}", DufojaAlteco / 2, DuonaLarĝeco,
                     DufojaAlteco);
               }
            }, {
               "progresivo", () => {
                  X += DuonaLarĝeco + Spaco + Spaco;
                  return string.Format("l {0} {1} l -{0} {1} m {2} -{3} l {0} {1} l -{0} {1}", DuonaLarĝeco,
                     DufojaAlteco / 2, Spaco, DufojaAlteco);
               }
            }, {
               "perfekto", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("l {0} {1} l -{0} {1} m {0} -{2} v {2}", DuonaLarĝeco, DufojaAlteco / 2,
                     DufojaAlteco);
               }
            }, {
               "estonteco", () => {
                  X += DuonaLarĝeco + Spaco;
                  return $"v {DufojaAlteco} l {DuonaLarĝeco} -{DufojaAlteco / 2} z";
               }
            }, {
               "sola", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("v {0} m 0 {1} v {0} m {2} -{3} v {0} m 0 {1} v {0}", Alteco, Spaceto,
                     DuonaLarĝeco, DufojaAlteco);
               }
            }, {
               "argumento2", () => {
                  X += DuonaLarĝeco + Spaco;
                  return string.Format("m {0} 0 l -{0} {1} l {0} {1} l {0} -{1} z", DuonaLarĝeco / 2,
                     DufojaAlteco / 2);
               }
            }, {
               "unueUjo2", () => {
                  X += DuonaLarĝeco + Spaceto * 2 + Spaco;
                  return string.Format("m 0 {0} v {0} m {1} -{0} v {0} m {1} -{2} l {3} {0} l -{3} {0}",
                     DufojaAlteco / 2, Spaceto, DufojaAlteco, DuonaLarĝeco);
               }
            }
         };
         _elirejo = elirejo;
         _xmlDokumento = new XDocument();
         Y = 130;
      }

      public void Vico() {
         XMax = (int) Math.Max(XMax, X);
         X = 10;
         Y += DufojaAlteco + Spaco * 4;
      }

      public override void Fini() {
         XNamespace ns = "http://www.w3.org/2000/svg";
         var svg = new XElement(ns + "svg");
         svg.SetAttributeValue(ns + "width", Math.Max(XMax, X));
         svg.SetAttributeValue(ns + "height", Y + DufojaAlteco + 2 * Spaceto);
         svg.SetAttributeValue(ns + "stroke-linecap", "round");

         var style = new XElement(ns + "style");
         style.SetAttributeValue(ns + "type", "text/css");
         style.Add(@"
.brush0 { fill: rgb(255,255,255); }
.pen0 { stroke: rgb(0,0,0); stroke-width: 1; stroke-linejoin: round; }
.font0 { font-size: 11px; font-family: ""MS Sans Serif""; }
.pen1 { stroke: rgb(0,0,0); stroke-width: <%= Dx %>; stroke-linejoin: round; }
.brush1 { fill: none; }
.font1 { font-weight: bold; font-size: 16px; font-family: System, sans-serif; }
");
         svg.Add(style);
         foreach (var vojo in Vojoj) {
            var path = new XElement(ns + "path");
            path.SetAttributeValue(ns + "class", "pen1");
            path.SetAttributeValue(ns + "fill", "none");
            path.SetAttributeValue(ns + "d", vojo);
            svg.Add(path);
         }

         _xmlDokumento.Add(svg);
         _xmlDokumento.Save(_elirejo);
      }

      private new const int DufojaAlteco = 258 - 130;
      private new const int Larĝeco = (int) (DufojaAlteco * 2 / (double) 5);

      private new int Alteco => DufojaAlteco / 2 - Spaceto / 2 - 5;

      private new int Spaceto => Dx / 2 + 10;
   }
}