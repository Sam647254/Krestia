using System;
using System.Collections.Generic;

namespace TimeranDesegnilo2 {
   public class RektangulaSvgDesegnilo : Desegnilo {
      protected sealed override Dictionary<string, LiteroDesegnilo> LiteroDesegniloj { get; set; }

      protected sealed override Dictionary<string, FinaĵoDesegnilo> FinaĵoDesegniloj { get; set; }

      private string NomoKomenco() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{0} z", DuonaLarĝeco, Dy, DuonaLarĝeco - Dx,
            DufojaAlteco - Dy * 2);
      }

      private string NomoFino() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("h {0} v {1} h -{0} v -{2} h {3} v -{4} h -{3} z", DuonaLarĝeco, DufojaAlteco, Dy,
            DuonaLarĝeco - Dx, DufojaAlteco - Dy * 2);
      }

      private string Lokokupilo() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("m 0 {0} h {1} v -{0} h {2} v {0} h {3} v -{0} h {2} v {4} h -{5} z", DufojaAlteco - Dy,
            DuonaLarĝeco / 2 - Dx / 2, Dx, DuonaLarĝeco / 2 - Dx * 3 / 2, DufojaAlteco, DuonaLarĝeco);
      }

      private string EcoDekstra() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m 0 {7} h -{0} v {8} h {0} z", DuonaLarĝeco,
            Alteco, DufojaAlteco, DufojaAlteco - Dy, DuonaLarĝeco - Dx, Alteco - Dy, DufojaAlteco - 2 * Dy,
            DufojaAlteco / 2 - Dy / 2, Dy);
      }

      private string EcoMaldekstra() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("a {0} {1} 0 0 1 0 {2} z v {3} a {4} {5} 0 0 0 0 -{6} z m 0 {7} h {0} v {8} h -{0} z",
            DuonaLarĝeco, Alteco, DufojaAlteco, DufojaAlteco - Dy, DuonaLarĝeco - Dx, Alteco - Dy,
            DufojaAlteco - 2 * Dy, Alteco - Dy / 2, Dy);
      }

      private string RekordoMaldekstra() {
         X += DuonaLarĝeco + Spaceto * 2 + Dx;
         return string.Format(
            "a {0} {1} 0 0 1 0 {2} z v {3} a {4} {5} 0 0 0 0 -{6} z m {7} 0 h {8} v {1} h -{8} z m 0 {9} h {8} v {1} h -{8} z",
            DuonaLarĝeco, Alteco, DufojaAlteco, DufojaAlteco - Dy, DuonaLarĝeco - Dx, Alteco - Dy,
            DufojaAlteco - 2 * Dy, DuonaLarĝeco + Spaceto, Dx, Alteco + Spaceto);
      }

      private string RekordoDekstra() {
         X += DuonaLarĝeco + Spaceto * 2 + Dx;
         return string.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} 0 h {8} v {1} h -{8} z m 0 {9} h {8} v {1} h -{8} z",
            DuonaLarĝeco, Alteco, DufojaAlteco, DufojaAlteco - Dy, DuonaLarĝeco - Dx, Alteco - Dy,
            DufojaAlteco - 2 * Dy, Spaceto, Dx, Alteco + Spaceto);
      }

      private string PredikativoEsti() {
         X += DuonaLarĝeco + Spaceto;
         return
            $"m 0 {DufojaAlteco / 2 - Dy / 2} h {DuonaLarĝeco} v {Dy} h -{DuonaLarĝeco} z m {DuonaLarĝeco / 2 - Dx / 2} -{DufojaAlteco / 2 - Dy / 2} h {Dx} v {DufojaAlteco} h -{Dx} z m {DuonaLarĝeco / 2} 0  h {Dx} v {DufojaAlteco} h -{Dx} z";
      }

      private string Pridiranto() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format(
            "m 0 {3} h {0} v {1} h -{0} z l {2} -{3} h {4} l -{2} {3} z m {2} -{3} h {4} l {2} {3} h -{4} z",
            DuonaLarĝeco, Dy, DuonaLarĝeco / 2 - Dx / 2, DufojaAlteco - Dy, Dx);
      }

      private string Etigo() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format(
            "m 0 {5} h {0} v {1} h -{0} z l {2} -{3} h {4} l -{2} {3} z m {2} -{3} h {4} l {2} {3} h -{4} z",
            DuonaLarĝeco, Dy, DuonaLarĝeco / 2 - Dx / 2, DufojaAlteco / 2 - Dy, Dx, DufojaAlteco - Dy);
      }

      private string Reflekcio() {
         X += Spaceto + Dx + DuonaLarĝeco;
         return string.Format("h {0} l {1} {2} h -{0} z m {3} 0 h {0} l -{1} {2} h -{0} z", Dx, DuonaLarĝeco,
            DufojaAlteco, DuonaLarĝeco);
      }

      private string Netransitiva() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("m 0 {0} h {1} v -{0} h {2} v {0} h {1} v {3} h -{1} v {0} h -{2} v -{0} h -{1} z",
            DufojaAlteco / 2 - Dy / 2, DuonaLarĝeco / 2 - Dx / 2, Dx, Dy);
      }

      private string NedirecktaTransitiva() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{0} v -{1} h {2} v -{3} h -{2} z", DuonaLarĝeco,
            Dy, DuonaLarĝeco / 2 - Dx / 2, DufojaAlteco - 2 * Dy);
      }

      private string Transitiva() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("h {0} v {1} l -{2} {3} h {2} v {1} h -{0} v -{1} l {2} -{3} h -{2} z", DuonaLarĝeco, Dy,
            DuonaLarĝeco - Dx, DufojaAlteco - 2 * Dy);
      }

      private string Dutransitiva() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format(
            "h {0} v {1} l -{2} {3} h {2} v {1} h -{0} v -{1} l {2} -{3} h -{2} z m 0 {4} h {0} v {1} h -{0} z",
            DuonaLarĝeco, Dy, DuonaLarĝeco - Dx, DufojaAlteco - 2 * Dy, DufojaAlteco / 2 - Dy / 2);
      }

      private string Perfekto() {
         X += Dx + DuonaLarĝeco / 2 + Spaceto * 2;
         return string.Format("h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z m {5} 0 h {0} v {1} h -{0} z", Dx,
            DufojaAlteco, Dx * 2, DuonaLarĝeco / 2, Alteco + Spaceto / 2, DuonaLarĝeco / 2 + Dx);
      }

      private string Progresivo() {
         X += Dx * 2 + DuonaLarĝeco / 2 + Spaceto * 2;
         return string.Format(
            "h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z", Dx,
            DufojaAlteco, Dx * 2, DuonaLarĝeco / 2, Alteco + Spaceto / 2);
      }

      private string Argumento1() {
         X += Dx * 2 + DuonaLarĝeco / 2 + Spaceto;
         return string.Format(
            "m {3} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z m {2} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z",
            Dx, DufojaAlteco, Dx * 2, DuonaLarĝeco / 2, Alteco + Spaceto / 2);
      }

      private string Argumento2() {
         X += Dx + DuonaLarĝeco + Spaceto;
         return string.Format(
            "m {3} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
            Dx, DufojaAlteco, Dx * 2, DuonaLarĝeco / 2, Alteco + Spaceto / 2);
      }

      private string Argumento3() {
         X += Dx * 2 + DuonaLarĝeco + Spaceto * 2;
         return string.Format(
            @"m {3} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z
m {1} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z", Dx, Dx + Spaceto, Dx * 2, DuonaLarĝeco / 2,
            Alteco + Spaceto / 2);
      }

      private string Estonteco() {
         X += Spaceto * 2 + DuonaLarĝeco / 2 + Dx * 2;
         return string.Format("h {0} v {1} h -{0} z m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z", Dx,
            DufojaAlteco, Dx * 2, DuonaLarĝeco / 2, Alteco + Spaceto / 2);
      }

      private string Igo() {
         X += Dx * 3 + DuonaLarĝeco + Spaceto;
         return string.Format(@"h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z m {5} 0 h {0} v {1} h -{0} z
m {5} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z", Dx, DufojaAlteco, Dx * 2, DuonaLarĝeco / 2,
            Alteco + Spaceto / 2, DuonaLarĝeco / 2 + Dx);
      }

      private string Translativo() {
         X += Spaceto + DuonaLarĝeco / 2 + Dx * 2;
         return
            $"h {Dx} v {DufojaAlteco} h -{Dx} z m {Dx + DuonaLarĝeco / 2} 0 h {Dx} l -{DuonaLarĝeco / 2} {Alteco + Spaceto / 2} l {DuonaLarĝeco / 2} {Alteco + Spaceto / 2} h -{Dx} l -{DuonaLarĝeco / 2} -{Alteco + Spaceto / 2} z";
      }

      private string Ekzistado() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format(
            "h {0} v {1} l -{2} {3} l {2} {3} v {1} h -{0} v -{1} h {2} l -{2} -{3} l {2} -{3} h -{2} z", DuonaLarĝeco,
            Dy, DuonaLarĝeco - Dx, DufojaAlteco / 2 - Dy);
      }

      private string Imperativo() {
         X += Dx * 3 + DuonaLarĝeco + Spaceto;
         return string.Format(@"h {0} v {1} h -{0} z m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z
m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z", Dx, DufojaAlteco, Dx * 2, DuonaLarĝeco / 2,
            Alteco + Spaceto / 2);
      }

      private string Invito() {
         X += Dx * 3 + DuonaLarĝeco + Spaceto;
         return string.Format(
            @"h {0} v {1} h -{0} z m 0 {5} h {0} v {1} h -{0} z m {2} -{5} h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z
m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z", Dx, Alteco, Dx * 2, DuonaLarĝeco / 2, Alteco + Spaceto / 2,
            Alteco + Spaceto);
      }

      private string Volo1() {
         X += DuonaLarĝeco + Spaceto * 2 + Dx;
         return string.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} {9} h {8} v {1} h -{8} z",
            DuonaLarĝeco, Alteco, DufojaAlteco, DufojaAlteco - Dy, DuonaLarĝeco - Dx, Alteco - Dy,
            DufojaAlteco - 2 * Dy, Spaceto, Dx, Alteco + Spaceto);
      }

      private string Volo2() {
         X += DuonaLarĝeco + Spaceto * 3 + Dx * 2;
         return string.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} {9} h {8} v {1} h -{8} z m {10} 0 h {8} v {1} h -{8} z",
            DuonaLarĝeco, Alteco, DufojaAlteco, DufojaAlteco - Dy, DuonaLarĝeco - Dx, Alteco - Dy,
            DufojaAlteco - 2 * Dy, Spaceto, Dx, Alteco + Spaceto, Dx + Spaceto);
      }

      private string Volo3() {
         X += DuonaLarĝeco + Spaceto * 4 + Dx * 3;
         return string.Format(
            @"m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} {9} h {8} v {1} h -{8} z
m {10} 0 h {8} v {1} h -{8} z m {10} 0 h {8} v {1} h -{8} z", DuonaLarĝeco, Alteco, DufojaAlteco, DufojaAlteco - Dy,
            DuonaLarĝeco - Dx, Alteco - Dy, DufojaAlteco - 2 * Dy, Spaceto, Dx, Alteco + Spaceto, Dx + Spaceto);
      }

      private string Sola() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format(
            "h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m -{2} {3} h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z",
            Dx, Alteco, DuonaLarĝeco - Dx, Alteco + Spaceto);
      }

      private string AktualaOkazo() {
         X += Dx * 3 + Spaceto;
         return string.Format("h {0} v {1} h {0} v -{1} h {0} v {2} h -{0} v -{1} h -{0} v {1} h -{0} z", Dx,
            (DufojaAlteco - Dy) / (double) 2, DufojaAlteco);
      }

      private string Okazo() {
         X += Dx * 3 + Spaceto;
         return string.Format("h {0} v {1} h -{0} z m {2} {3} v {4} h {2} v -{4} z", Dx * 3, DufojaAlteco, Dx, Dy,
            DufojaAlteco - 2 * Dy);
      }

      private string FinitaOkazo() {
         X += Dx * 4 + Spaceto * 2;
         return string.Format("h {0} v {1} h -{0} z m {2} 0 h {3} v {1} h -{3} z m {0} {4} v {5} h {0} v -{5} z", Dx,
            DufojaAlteco, Dx + Spaceto, Dx * 3, Dy, DufojaAlteco - 2 * Dy);
      }

      private string Klaso() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("m {0} 0 h {1} l -{0} {2} l {0} {2} h -{1} l -{0} -{2} z", DuonaLarĝeco - Dx, Dx,
            DufojaAlteco / 2);
      }

      private string Difinito() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("m 0 {0} l {1} -{0} h {3} l -{1} {0} h {1} v {2} h -{1} l {1} {0} h -{3} l -{1} -{0} z",
            DufojaAlteco / 2 - Dy / 2, DuonaLarĝeco - Dx, Dy, Dx);
      }

      private string PredikatoDe() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format(
            "h {0} l {1} {2} h -{0} z m 0 {2} h {3} v {4} h -{3} z m 0 {5} l {1} -{2} h {0} l -{1} {2} z",
            Dx, DuonaLarĝeco - Dx, DufojaAlteco / 2 - Dy / 2, DuonaLarĝeco, Dy, DufojaAlteco / 2 + Dy / 2);
      }

      private string UnuNombro() {
         X += DuonaLarĝeco + Spaceto * 2 + Dx;
         return string.Format(
            "m 0 {0} l {1} -{0} h {3} l -{1} {0} h {1} v {2} h -{1} l {1} {0} h -{3} l -{1} -{0} z m {4} -{0} h {3} v {5} h -{3} z",
            DufojaAlteco / 2 - Dy / 2, DuonaLarĝeco - Dx, Dy, Dx, DuonaLarĝeco + Spaceto, Alteco);
      }

      private string PluraNombro() {
         X += DuonaLarĝeco + Spaceto * 2 + Dx;
         return string.Format(
            @"m 0 {0} l {1} -{0} h {3} l -{1} {0} h {1} v {2} h -{1} l {1} {0} h -{3} l -{1} -{0} z m {4} -{0} h {3} v {5} h -{3} z
m 0 {6} h {3} v {5} h -{3} z", DufojaAlteco / 2 - Dy / 2, DuonaLarĝeco - Dx, Dy, Dx, DuonaLarĝeco + Spaceto, Alteco,
            Spaceto + Alteco);
      }

      private string Havaĵo() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("m 0 {0} h {1} v -{0} h {2} v {3} h -{2} v -{0} h -{1} z", DufojaAlteco / 2 - Dy / 2,
            DuonaLarĝeco - Dx, Dx, DufojaAlteco);
      }

      private string AtributivoEstiMaldekstra() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("m 0 {0} h {1} v -{0} h {2} v {3} h -{2} v -{0} h -{1} v -{4} h {1} v -{4} h -{1} z",
            (DufojaAlteco - 3 * Dy) / 2, DuonaLarĝeco - Dx, Dx, DufojaAlteco, Dy);
      }

      private string AtributivoEstiDekstra() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("h {2} v {0} h {1} v {4} h -{1} v {4} h {1} v {4} h -{1} v {0} h -{2} z",
            (DufojaAlteco - 3 * Dy) / 2, DuonaLarĝeco - Dx, Dx, DufojaAlteco, Dy);
      }

      private string Ĝerundo() {
         X += DuonaLarĝeco + Dx * 2 + Spaceto;
         return string.Format(
            @"h {3} l {1} {0} h {3} l -{1} -{0} h {3} l {1} {0} v {2} l -{1} {0} h -{3} l {1} -{0} h -{3} l -{1} {0}
h -{3} l {1} -{0} h -{1} v -{2} h {1} z", DufojaAlteco / 2 - Dy / 2, DuonaLarĝeco - Dx, Dy, Dx);
      }

      private string Havado() {
         X += DuonaLarĝeco + Dx * 2 + Spaceto;
         return string.Format(@"m {1} 0 h {3} l -{1} {0} h {3} l {1} -{0} h {3} l -{1} {0} v {2} l {1} {0} h -{3}
l -{1} -{0} h -{3} l {1} {0} h -{3} l -{1} -{0} v -{2} z", DufojaAlteco / 2 - Dy / 2, DuonaLarĝeco - Dx, Dy, Dx);
      }

      private string MalplenaVerbo() {
         X += DuonaLarĝeco + Spaceto * 2;
         return string.Format(@"h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{5} {6} h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z", 0, Dx, DuonaLarĝeco / 2, Alteco,
            DuonaLarĝeco + Dx, DuonaLarĝeco + Dx, Alteco + Spaceto);
      }

      private string OblikaNetransitiva() {
         X += DuonaLarĝeco + Spaceto * 2;
         return string.Format(@"h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{4} {5} h {4} v {7} h -{8} v {6} h -{1} v -{6} h -{8} z", 0, Dx, DuonaLarĝeco / 2, Alteco, DuonaLarĝeco + Dx,
            Alteco + Spaceto, Alteco - Dy, Dy, (DuonaLarĝeco + Dx) / 2 - Dx / 2);
      }

      private string OblikaTransitiva() {
         X += DuonaLarĝeco + Spaceto * 2;
         return string.Format(@"h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{4} {5} h {4} v {7} h -{8} v {6} h {8} v {7} h -{4} v -{7} h {8} v -{6} h -{8} z", 0, Dx, DuonaLarĝeco / 2, Alteco,
            DuonaLarĝeco + Dx, Alteco + Spaceto, Alteco - Dy * 2, Dy, DuonaLarĝeco / 2 - Dx / 2 + Spaceto / 2);
      }

      private string NedirektaNetransitiva() {
         X += DuonaLarĝeco + Spaceto * 2;
         return string.Format(@"h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{2} {5} v {6} h {8} v {7} h -{4} v -{7} h {2} v -{6} z", 0, Dx, DuonaLarĝeco / 2, Alteco, DuonaLarĝeco + Dx,
            Alteco + Spaceto, Alteco - Dy, Dy, DuonaLarĝeco / 2 + Spaceto / 2 - Dx / 2);
      }

      private string ModifantoMaldekstra() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("h {0} v {1} h -{4} v -{3} h -{2} z", DuonaLarĝeco, DufojaAlteco, DuonaLarĝeco - Dx,
            DufojaAlteco - Dy, Dx);
      }

      private string ModifantoDekstra() {
         X += DuonaLarĝeco + Spaceto;
         return $"h {DuonaLarĝeco} v {Dy} h -{DuonaLarĝeco - Dx} v {DufojaAlteco - Dy} h -{Dx} z";
      }

      private string NombrigeblaEcoDekstra() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{2} v {3} h {2} v {1} h -{0} z", DuonaLarĝeco,
            Dy, DuonaLarĝeco - Dx, Alteco + Spaceto / 2 - Dy * 3 / 2);
      }

      private string NombrigeblaEcoMaldekstra() {
         X += DuonaLarĝeco + Spaceto;
         return string.Format("h {0} v {1} h -{0} v -{2} h {3} v -{4} h -{3} v {2} h {3} v -{4} h -{3} z", DuonaLarĝeco,
            DufojaAlteco, Dy, DuonaLarĝeco - Dx, Alteco + Spaceto / 2 - Dy / 2);
      }

      private string MankaNominativo() {
         X += Dx + Spaceto + DuonaLarĝeco + Spaceto;
         return string.Format("h {0} v {1} h -{0} z m {2} 0 h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z", Dx,
            DufojaAlteco / 2, Dx + Spaceto, DuonaLarĝeco - Dx);
      }

      private string Ujo1Unue() {
         X += Dx + Spaceto + DuonaLarĝeco + Spaceto;
         return string.Format("m 0 {4} h {0} v {1} h -{0} z m {2} -{4} h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
            Dx, DufojaAlteco / 2, Dx + Spaceto, DuonaLarĝeco - Dx, Alteco + Spaceto / 2);
      }

      private string Ujo2Unue() {
         X += (Dx + Spaceto) * 2 + DuonaLarĝeco + Spaceto;
         return string.Format(@"m 0 {4} h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m {2} -{4}
h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z", Dx, DufojaAlteco / 2, Dx + Spaceto, DuonaLarĝeco - Dx,
            Alteco + Spaceto / 2);
      }

      private string Ujo3Unue() {
         X += (Dx + Spaceto) * 3 + DuonaLarĝeco + Spaceto;
         return string.Format(
            @"m 0 {4} h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m {2} -{4}
h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z", Dx, DufojaAlteco / 2, Dx + Spaceto, DuonaLarĝeco - Dx,
            Alteco + Spaceto / 2);
      }

      private string Vokativo() {
         X += Dx + Spaceto + DuonaLarĝeco + Spaceto;
         return string.Format(@"h {0} v {5} h -{0} z m {1} 0 h {0} v {2} h {3} v {4} h -{3} v {2} h -{0} z",
            Dx, Dx + Spaceto, DufojaAlteco / 2 - Dy / 2, DuonaLarĝeco - Dx, Dy, DufojaAlteco);
      }

      private string Argumento2De() {
         X += (Dx + Spaceto) * 2 + DuonaLarĝeco + Spaceto;
         return string.Format(
            @"h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m {2} 0 h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
            Dx, DufojaAlteco / 2, Dx + Spaceto, DuonaLarĝeco - Dx);
      }

      private string Vico() {
         DokumentoLarĝeco = (int) Math.Max(DokumentoLarĝeco, X + Spaco);
         X = Spaco;
         Y += DufojaAlteco + Spaco;
         return "";
      }

      private string Cifero() {
         X += Dx + Spaceto;
         return string.Format("h {0} v {1} h -{0} z", Dx, DufojaAlteco);
      }

      private string M(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {1} h {2} v {3} h -{4} v -{5}", Dx, aktualaAlteco - Dy, aktualaLarĝeco - Dx, Dy,
            aktualaLarĝeco, aktualaAlteco);
      }

      private string P(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {1} h -{2} v {3} h -{4} v -{5}", aktualaLarĝeco, Dy, aktualaLarĝeco - Dx,
            aktualaAlteco - Dy, Dx, aktualaAlteco);
      }

      private string Pl(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {4} h -{6} v -{2} h -{5} v {2} h -{3} z m {9} {7} v {8} h {1} v -{8} z",
            aktualaLarĝeco, aktualaLarĝeco / 5, aktualaAlteco - Dy, Dx, aktualaAlteco,
            aktualaLarĝeco - (aktualaLarĝeco / 5 + Dx * 3), (aktualaLarĝeco / 5 + Dx * 2), Dy, aktualaAlteco - 2 * Dy,
            aktualaAlteco - aktualaAlteco / 5 - Dx * 3);
      }

      private string Pr(int aktualaLarĝeco, int aktualaAlteco) {
         var rSpaco = aktualaLarĝeco / 5;
         return string.Format(
            "v {4} h {3} v -{2} h {1} v -{5} z m {0} 0 m 0 {5} l -{6} {2} h -{3} l {6} -{2} z m -{7} 0 l {6} {2} h -{3} l -{6} -{2} z",
            aktualaLarĝeco, aktualaLarĝeco - Dx, aktualaAlteco - Dy, Dx, aktualaAlteco, Dy, rSpaco / 2 + Dx / 2,
            rSpaco + Dx);
      }

      private string B(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {4} h -{5} v {2} h -{3} v -{2} h -{1} v {2} h -{3} z", aktualaLarĝeco, Dx * 2,
            aktualaAlteco - Dy, Dx, Dy, aktualaLarĝeco - Dx * 4);
      }

      private string Bl(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format(
            "h {0} v {4} h -{6} v -{2} h -{5} v {2} h -{3} v -{2} h -{1} v {2} h -{3} z m {9} {7} v {8} h {1} v -{8} z",
            aktualaLarĝeco, aktualaLarĝeco / 5, aktualaAlteco - Dy, Dx, aktualaAlteco,
            aktualaLarĝeco - 2 * (aktualaLarĝeco / 5 + Dx * 2), (aktualaLarĝeco / 5 + Dx * 2), Dy,
            aktualaAlteco - 2 * Dy, aktualaAlteco - aktualaAlteco / 5 - Dx * 3);
      }

      private string Br(int aktualaLarĝeco, int aktualaAlteco) {
         var rSpaco = aktualaLarĝeco / 5;
         return string.Format(@"v {4} h {3} v -{2} h {8} v {2} h {3} v -{2} h {1} v -{5} z m {0} 0 m 0 {5}
l -{6} {2} h -{3} l {6} -{2} z m -{7} 0 l {6} {2} h -{3} l -{6} -{2} z", aktualaLarĝeco,
            aktualaLarĝeco - (aktualaLarĝeco / 5 + 2 * Dx), aktualaAlteco - Dy, Dx, aktualaAlteco, Dy,
            rSpaco / 2 + Dx / 2,
            rSpaco + Dx, aktualaLarĝeco / 5);
      }

      private string V(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {1} h -{0} z m 0 {1} l {2} {3} h {4} l -{2} -{3} z", aktualaLarĝeco, Dy,
            aktualaLarĝeco < Larĝeco ? aktualaLarĝeco - Dx : aktualaLarĝeco / 2 - Dx / 2, aktualaAlteco - Dy, Dx);
      }

      private string K(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {4} h -{3} v -{2} h -{1} z", aktualaLarĝeco, aktualaLarĝeco - Dx,
            aktualaAlteco - Dy, Dx, aktualaAlteco);
      }

      private string Kl(int aktualaLarĝeco, int aktualaAlteco) {
         var l = aktualaLarĝeco / 5;
         var lLarĝeco = l + 2 * Dx;
         return string.Format("h {0} v {1} h -{2} v -{3} h -{4} v {3} h -{5} z m {2} {6} v {7} h {8} v -{7} z",
            aktualaLarĝeco, aktualaAlteco, Dx, aktualaAlteco - Dy, aktualaLarĝeco - lLarĝeco - Dx, lLarĝeco, Dy,
            aktualaAlteco - Dy * 2, l);
      }

      private string Kr(int aktualaLarĝeco, int aktualaAlteco) {
         var rSpaco = aktualaLarĝeco / 5;
         return string.Format(
            "h {0} v {4} h -{3} v -{2} h -{1} z m 0 {5} l {6} {2} h {3} l -{6} -{2} z m {7} 0 l -{6} {2} h {3} l {6} -{2} z",
            aktualaLarĝeco, aktualaLarĝeco - Dx, aktualaAlteco - Dy, Dx, aktualaAlteco, Dy, rSpaco / 2 + Dx / 2,
            rSpaco + Dx);
      }

      private string G(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {4} h -{3} v -{2} h -{1} v {2} h -{3} v -{2} h -{5} z", aktualaLarĝeco, Dx * 2,
            aktualaAlteco - Dy, Dx, aktualaAlteco, aktualaLarĝeco - Dx * 4);
      }

      private string Gr(int aktualaLarĝeco, int aktualaAlteco) {
         var rSpaco = aktualaLarĝeco / 5;
         return string.Format(
            @"h {0} v {4} h -{3} v -{2} h -{1} v {2} h -{3} v -{2} h -{8} z m 0 {5} l {6} {2} h {3} l -{6} -{2} z m {7} 0
l -{6} {2} h {3} l {6} -{2} z", aktualaLarĝeco, rSpaco, aktualaAlteco - Dy, Dx, aktualaAlteco, Dy, rSpaco / 2 + Dx / 2,
            rSpaco + Dx, aktualaLarĝeco - rSpaco - 2 * Dx);
      }

      private string Gl(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format(
            "h {0} v {4} h -{3} v -{2} h -{1} v {2} h -{3} v -{2} h -{5} v {2} h -{6} z m {3} {7} v {8} h {1} v -{8} z",
            aktualaLarĝeco, aktualaLarĝeco / 5, aktualaAlteco - Dy, Dx, aktualaAlteco,
            aktualaLarĝeco - 2 * (aktualaLarĝeco / 5 + Dx * 2), aktualaLarĝeco / 5 + Dx * 2, Dy,
            aktualaAlteco - 2 * Dy);
      }

      private string E(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("m {4} {1} h -{2} v -{1} h -{0} v {3} h {0} v -{1} h {2} z", Dx, aktualaAlteco / 2 - Dy / 2,
            aktualaLarĝeco - Dx, aktualaAlteco, aktualaLarĝeco);
      }

      private string I(int aktualaLarĝeco, int aktualaAlteco) {
         return $"h {aktualaLarĝeco} v {Dy} h -{aktualaLarĝeco - Dx} v {aktualaAlteco - Dy} h -{Dx} z";
      }

      private string O(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("m 0 {1} h {2} v -{1} h {0} v {3} h -{0} v -{1} h -{2} z", Dx, aktualaAlteco / 2 - Dy / 2,
            aktualaLarĝeco - Dx, aktualaAlteco);
      }

      private string AA(int aktualaLarĝeco, int aktualaAlteco) {
         return
            $"m 0 {aktualaAlteco - Dy} h {aktualaLarĝeco - Dx} v -{aktualaAlteco - Dy} h {Dx} v {aktualaAlteco} h -{aktualaLarĝeco} z";
      }

      private string N(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("m 0 {0} h {1} v -{0} h {2} v {0} h {1} v {3} h -{4}", aktualaAlteco - Dy,
            aktualaLarĝeco / 2 - Dx / 2, Dx, Dy, aktualaLarĝeco);
      }

      private string H(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {1} h -{0} z m 0 {2} h {0} v {1} h -{0} z", aktualaLarĝeco, Dy,
            aktualaAlteco - Dy);
      }

      private string T(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {1} h -{2} v {3} h -{4} v -{3} h -{2} z", aktualaLarĝeco, Dy,
            aktualaLarĝeco / 2 - Dx / 2, aktualaAlteco - Dy, Dx);
      }

      private string Tl(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {1} h -{2} v {3} h -{4} v -{3} h -{2} z m {5} {1} v {6} h {7} v -{6} z",
            aktualaLarĝeco, Dy, aktualaLarĝeco / 2 - (aktualaLarĝeco / 5 + 2 * Dx) / 2, aktualaAlteco - Dy,
            aktualaLarĝeco / 5 + 2 * Dx, aktualaLarĝeco / 2 - (aktualaLarĝeco / 5 + 2 * Dx) / 2 + Dx,
            aktualaAlteco - 2 * Dy, aktualaLarĝeco / 5);
      }

      private string Tr(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {1} h -{0} z m {2} 0 h {3} l {5} {4} h -{3} z m {6} 0 h {3} l -{5} {4} h -{3} z",
            aktualaLarĝeco, Dy, aktualaLarĝeco / 2 - (aktualaLarĝeco / 5 + 2 * Dx) / 2, Dx, aktualaAlteco,
            aktualaLarĝeco / 10 + Dx / 2, aktualaLarĝeco / 5 + Dx);
      }

      private string D(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {1} h -{2} v {5} h -{3} v -{5} h -{4} v {5} h -{3} v -{5} h -{2} z",
            aktualaLarĝeco, Dy, aktualaLarĝeco / 2 - 2 * Dx, Dx, 2 * Dx, aktualaAlteco - Dy);
      }

      private string Dl(int aktualaLarĝeco, int aktualaAlteco) {
         var l = aktualaLarĝeco / 5;
         var lLarĝeco = aktualaLarĝeco / 5 + 2 * Dx;
         return string.Format(
            "h {0} v {1} h -{2} v -{3} h -{4} v {3} h -{2} z m {5} {6} v {7} h {8} v -{7} z m {9} 0 v {7} h {8} v -{7} z",
            aktualaLarĝeco, aktualaAlteco, lLarĝeco, aktualaAlteco - Dy, aktualaLarĝeco - 2 * lLarĝeco, Dx, Dy,
            aktualaAlteco - 2 * Dy, l, l + Dx * 2 + aktualaLarĝeco - 2 * lLarĝeco);
      }

      private string Dr(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format(
            @"h {0} v {1} h -{0} z m 0 {1} l {2} {3} h {4} l -{2} -{3} z m {2} 0 v {3} h {4} v -{3} z m {5} 0
h {4} v {3} h -{4} z m 0 {3} l {2} -{3} h {4} l -{2} {3} z", aktualaLarĝeco, Dy, aktualaLarĝeco / 2 - Dx * 2,
            aktualaAlteco - Dy, Dx, Dx * 3);
      }

      private string S(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format(
            "h {0} v {1} h -{0} z m 0 {2} l {3} -{5} h {4} l -{3} {5} z m {3} -{5} h {4} l {3} {5} h -{4}",
            aktualaLarĝeco, Dy, aktualaAlteco, aktualaLarĝeco / 2 - Dx / 2, Dx, aktualaAlteco - Dy);
      }

      private string R(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format(
            "h {0} v {1} h -{0} z m 0 {1} l {2} {3} h {4} l -{2} -{3} z m {0} 0 h -{4} l -{2} {3} h {4} z",
            aktualaLarĝeco, Dy, aktualaLarĝeco / 2 - Dx / 2, aktualaAlteco - Dy, Dx);
      }

      private string L(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {1} h -{0} z m {2} {3} v {4} h {5} v -{4} z", aktualaLarĝeco, aktualaAlteco, Dx,
            Dy, aktualaAlteco - 2 * Dy, aktualaLarĝeco - 2 * Dx);
      }

      private string J(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{0} v -{1} h {2} v -{3} h -{2} z",
            aktualaLarĝeco, Dy, aktualaLarĝeco / 2 - Dx / 2, aktualaAlteco - Dy * 2);
      }

      private string W(int aktualaLarĝeco, int aktualaAlteco) {
         return string.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} v -{3} a {4} {5} 0 0 1 0 -{6} z a {0} {1} 0 0 1 0 {2} v -{3} a {4} {5} 0 0 0 0 -{6} z",
            aktualaLarĝeco / 2, aktualaAlteco / 2, aktualaAlteco, Dy, aktualaLarĝeco / 2 - Dx, aktualaAlteco / 2 - Dy,
            aktualaAlteco - Dy * 2);
      }

      public RektangulaSvgDesegnilo(string elirejo, int alteco, int larĝeco, int dx, int dy, int spaco = 8) : base(
         elirejo, alteco, larĝeco, dx, dy, spaco) {
         FinaĵoDesegniloj = new Dictionary<string, FinaĵoDesegnilo> {
            {"[", NomoKomenco}, {"]", NomoFino}, {"lokokupilo", Lokokupilo}, {"CountableNoun", Difinito},
            {"antaŭNenombrigeblaEco", EcoDekstra}, {"malantaŭNenombrigeblaEco", EcoMaldekstra},
            {"rekordo<", RekordoMaldekstra}, {"rekordo>", RekordoDekstra}, {"senfara", Pridiranto},
            {"atributivoEstiMalantaŭ", AtributivoEstiMaldekstra}, {"atributivoEstiAntaŭ", AtributivoEstiDekstra},
            {"predikativoEsti", Klaso}, {"vico", Vico}, {"netransitivaVerbo", Netransitiva},
            {"nedirektaTransitivaVerbo", NedirecktaTransitiva}, {"transitivaVerbo", Transitiva},
            {"dutransitivaVerbo", Dutransitiva}, {"imperativo", Imperativo}, {"invito", Invito}, {"difinito", Difinito},
            {"unuNombro", UnuNombro}, {"pluraNombro", PluraNombro}, {"havaĵo", Havaĵo},
            {"malplenaVerbo", MalplenaVerbo}, {"oblikaNetransitivaVerbo", OblikaNetransitiva},
            {"oblikaTransitivaVerbo", OblikaTransitiva}, {"nedirektaNetransitivaVerbo", NedirektaNetransitiva},
            {"progresivo", Progresivo}, {"perfekto", Perfekto}, {"volo1", Volo1}, {"volo2", Volo2}, {"volo3", Volo3},
            {"sola", Sola}, {"intenco", Estonteco}, {"Modifier", ModifantoMaldekstra},
            {"antaŭModifanto", ModifantoDekstra}, {"antaŭNombrigeblaEco", NombrigeblaEcoDekstra},
            {"malantaŭNombrigeblaEco", NombrigeblaEcoMaldekstra}, {"mankaNominativo", MankaNominativo},
            {"ĝerundo", Ĝerundo}, {"ekzistado", Ekzistado}, {"havado", Havado}, {"argumento1", Argumento1},
            {"argumento2", Argumento2}, {"argumento3", Argumento3}, {"translativo", Translativo},
            {"ujo1Unue", Ujo1Unue}, {"unueUjo2", Ujo2Unue}, {"unueUjo3", Ujo3Unue}, {"igo", Igo}, {"etigo", Etigo},
            {"reflekcio", Reflekcio}, {"hipoteza", Okazo}, {"apartigita", AktualaOkazo}, {"finitaOkazo", FinitaOkazo},
            {"cifero", Cifero}, {"vokativo", Vokativo}, {"argumento2De", Argumento2De}, {"predikatoDe", PredikatoDe}
         };
         LiteroDesegniloj = new Dictionary<string, LiteroDesegnilo> {
            {"m", M}, {"p", P}, {"pl", Pl}, {"pr", Pr}, {"b", B}, {"bl", Bl}, {"br", Br}, {"v", V}, {"n", N}, {"t", T},
            {"tl", Tl}, {"tr", Tr}, {"d", D}, {"dl", Dl}, {"dr", Dr}, {"s", S}, {"l", L}, {"r", R}, {"j", J}, {"k", K},
            {"kl", Kl}, {"kr", Kr}, {"g", G}, {"gr", Gr}, {"gl", Gl}, {"h", H}, {"a", M}, {"ɒ", AA}, {"e", E}, {"i", I},
            {"o", O}, {"u", K}, {"w", W}
         };
      }
   }
}