Imports System.Xml

Public Class RektangulaSvgDesegnilo
   Inherits Desegnilo

   Protected Overrides Property LiteroDesegniloj As Dictionary(Of String, LiteroDesegnilo) =
                       New Dictionary(Of String, LiteroDesegnilo) From {
      {"m", AddressOf M},
      {"p", AddressOf P},
      {"pl", AddressOf Pl},
      {"pr", AddressOf Pr},
      {"b", AddressOf B},
      {"bl", AddressOf Bl},
      {"br", AddressOf Br},
      {"v", AddressOf V},
      {"n", AddressOf N},
      {"t", AddressOf T},
      {"tl", AddressOf Tl},
      {"tr", AddressOf Tr},
      {"d", AddressOf D},
      {"dl", AddressOf Dl},
      {"dr", AddressOf Dr},
      {"s", AddressOf S},
      {"l", AddressOf L},
      {"r", AddressOf R},
      {"j", AddressOf J},
      {"k", AddressOf K},
      {"kl", AddressOf Kl},
      {"kr", AddressOf Kr},
      {"g", AddressOf G},
      {"gr", AddressOf Gr},
      {"gl", AddressOf Gl},
      {"h", AddressOf H},
      {"a", AddressOf M},
      {"ɒ", AddressOf AA},
      {"e", AddressOf E},
      {"i", AddressOf I},
      {"o", AddressOf O},
      {"u", AddressOf K},
      {"w", AddressOf W}
      }

   Protected Overrides Property FinaĵoDesegniloj As Dictionary(Of String, FinaĵoDesegnilo) =
                       New Dictionary(Of String, FinaĵoDesegnilo) From {
      {"[", AddressOf NomoKomenco},
      {"]", AddressOf NomoFino},
      {"lokokupilo", AddressOf Lokokupilo},
      {"klaso", AddressOf Difinito},
      {"antaŭNenombrigeblaEco", AddressOf EcoDekstra},
      {"malantaŭNenombrigeblaEco", AddressOf EcoMaldekstra},
      {"rekordo<", AddressOf RekordoMaldekstra},
      {"rekordo>", AddressOf RekordoDekstra},
      {"pridiranto", AddressOf Pridiranto},
      {"atributivoEstiMalantaŭ", AddressOf AtributivoEstiMaldekstra},
      {"atributivoEstiAntaŭ", AddressOf AtributivoEstiDekstra},
      {"predikativoEsti", AddressOf Klaso},
      {"vico", AddressOf Vico},
      {"netransitivaVerbo", AddressOf Netransitiva},
      {"nedirektaTransitivaVerbo", AddressOf NedirecktaTransitiva},
      {"transitivaVerbo", AddressOf Transitiva},
      {"dutransitivaVerbo", AddressOf Dutransitiva},
      {"imperativo", AddressOf Imperativo},
      {"invito", AddressOf Invito},
      {"difinito", AddressOf Difinito},
      {"unuNombro", AddressOf UnuNombro},
      {"pluraNombro", AddressOf PluraNombro},
      {"havaĵo", AddressOf Havaĵo},
      {"malplenaVerbo", AddressOf MalplenaVerbo},
      {"oblikaNetransitivaVerbo", AddressOf OblikaNetransitiva},
      {"oblikaTransitivaVerbo", AddressOf OblikaTransitiva},
      {"nedirektaNetransitivaVerbo", AddressOf NedirektaNetransitiva},
      {"progresivo", AddressOf Progresivo},
      {"perfekto", AddressOf Perfekto},
      {"volo1", AddressOf Volo1},
      {"volo2", AddressOf Volo2},
      {"volo3", AddressOf Volo3},
      {"sola", AddressOf Sola},
      {"intenco", AddressOf Estonteco},
      {"malantaŭModifanto", AddressOf ModifantoMaldekstra},
      {"antaŭModifanto", AddressOf ModifantoDekstra},
      {"antaŭNombrigeblaEco", AddressOf NombrigeblaEcoDekstra},
      {"malantaŭNombrigeblaEco", AddressOf NombrigeblaEcoMaldekstra},
      {"mankaNominativo", AddressOf MankaNominativo},
      {"ĝerundo", AddressOf Ĝerundo},
      {"ekzistado", AddressOf Ekzistado},
      {"havado", AddressOf Havado},
      {"argumento1", AddressOf Argumento1},
      {"argumento2", AddressOf Argumento2},
      {"argumento3", AddressOf Argumento3},
      {"translativo", AddressOf Translativo},
      {"ujo1Unue", AddressOf Ujo1Unue},
      {"unueUjo2", AddressOf Ujo2Unue},
      {"unueUjo3", AddressOf Ujo3Unue},
      {"igo", AddressOf Igo},
      {"etigo", AddressOf Etigo},
      {"reflekcio", AddressOf Reflekcio},
      {"hipoteza", AddressOf Okazo},
      {"apartigita", AddressOf AktualaOkazo},
      {"finitaOkazo", AddressOf FinitaOkazo},
      {"cifero", AddressOf Cifero}
   }
   
   Private Function NomoKomenco() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{0} z",
                           DuonaLarĝeco, Dy, DuonaLarĝeco - Dx, DufojaAlteco - Dy*2)
   End Function

   Private Function NomoFino() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{0} v -{2} h {3} v -{4} h -{3} z",
                           DuonaLarĝeco, DufojaAlteco, Dy, DuonaLarĝeco - Dx, DufojaAlteco - Dy*2)
   End Function

   Private Function Lokokupilo() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {0} h {3} v -{0} h {2} v {4} h -{5} z",
                           DufojaAlteco - Dy, DuonaLarĝeco\2 - Dx\2, Dx, DuonaLarĝeco\2 - Dx*3\2, DufojaAlteco,
                           DuonaLarĝeco)
   End Function

   Private Function EcoDekstra() As String
      X += DuonaLarĝeco + Spaceto
      Return _
         String.Format("m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m 0 {7} h -{0} v {8} h {0} z",
                       DuonaLarĝeco, Alteco, DufojaAlteco, DufojaAlteco - Dy, DuonaLarĝeco - Dx, Alteco - Dy,
                       DufojaAlteco - 2*Dy,
                       Alteco - Dy\2, Dy)
   End Function

   Private Function EcoMaldekstra() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("a {0} {1} 0 0 1 0 {2} z v {3} a {4} {5} 0 0 0 0 -{6} z m 0 {7} h {0} v {8} h -{0} z",
                           DuonaLarĝeco, Alteco, DufojaAlteco, DufojaAlteco - Dy, DuonaLarĝeco - Dx,
                           Alteco - Dy,
                           DufojaAlteco - 2*Dy,
                           Alteco - Dy\2, Dy)
   End Function

   Private Function RekordoMaldekstra() As String
      X += DuonaLarĝeco + Spaceto*2 + Dx
      Return _
         String.Format(
            "a {0} {1} 0 0 1 0 {2} z v {3} a {4} {5} 0 0 0 0 -{6} z m {7} 0 h {8} v {1} h -{8} z m 0 {9} h {8} v {1} h -{8} z",
            DuonaLarĝeco, Alteco, DufojaAlteco, DufojaAlteco - Dy, DuonaLarĝeco - Dx, Alteco - Dy,
            DufojaAlteco - 2*Dy, DuonaLarĝeco + Spaceto, Dx, Alteco + Spaceto)
   End Function

   Private Function RekordoDekstra() As String
      X += DuonaLarĝeco + Spaceto*2 + Dx
      Return _
         String.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} 0 h {8} v {1} h -{8} z m 0 {9} h {8} v {1} h -{8} z",
            DuonaLarĝeco, Alteco, DufojaAlteco, DufojaAlteco - Dy, DuonaLarĝeco - Dx, Alteco - Dy,
            DufojaAlteco - 2*Dy, Spaceto, Dx, Alteco + Spaceto)
   End Function

   Private Function PredikativoEsti() As String
      X += DuonaLarĝeco + Spaceto
      Return _
         String.Format("m 0 {3} h {5} v {6} h -{5} z m {1} -{3} h {2} v {4} h -{2} z m {7} 0  h {2} v {4} h -{2} z",
                       DufojaAlteco - Dy, DuonaLarĝeco\2 - Dx\2, Dx, DufojaAlteco\2 - Dy\2, DufojaAlteco,
                       DuonaLarĝeco, Dy, DuonaLarĝeco\2)
   End Function

   Private Function Pridiranto() As String
      X += DuonaLarĝeco + Spaceto
      Return _
         String.Format(
            "m 0 {3} h {0} v {1} h -{0} z l {2} -{3} h {4} l -{2} {3} z m {2} -{3} h {4} l {2} {3} h -{4} z",
            DuonaLarĝeco, Dy, DuonaLarĝeco\2 - Dx\2, DufojaAlteco - Dy, Dx)
   End Function

   Private Function Etigo() As String
      X += DuonaLarĝeco + Spaceto
      Return _
         String.Format(
            "m 0 {5} h {0} v {1} h -{0} z l {2} -{3} h {4} l -{2} {3} z m {2} -{3} h {4} l {2} {3} h -{4} z",
            DuonaLarĝeco, Dy, DuonaLarĝeco\2 - Dx\2, DufojaAlteco\2 - Dy, Dx, DufojaAlteco - Dy)
   End Function
   
   Private Function Reflekcio() As String
      X += Spaceto + Dx + DuonaLarĝeco
      Return String.Format(
         "h {0} l {1} {2} h -{0} z m {3} 0 h {0} l -{1} {2} h -{0} z",
         Dx, DuonaLarĝeco, DufojaAlteco, DuonaLarĝeco)
   End Function

   Private Function Netransitiva() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {0} h {1} v {3} h -{1} v {0} h -{2} v -{0} h -{1} z",
                           DufojaAlteco\2 - Dy\2, DuonaLarĝeco\2 - Dx\2, Dx, Dy)
   End Function

   Private Function NedirecktaTransitiva() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{0} v -{1} h {2} v -{3} h -{2} z",
                           DuonaLarĝeco, Dy, DuonaLarĝeco\2 - Dx\2, DufojaAlteco - 2*Dy)
   End Function

   Private Function Transitiva() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} l -{2} {3} h {2} v {1} h -{0} v -{1} l {2} -{3} h -{2} z",
                           DuonaLarĝeco, Dy, DuonaLarĝeco - Dx, DufojaAlteco - 2*Dy)
   End Function

   Private Function Dutransitiva() As String
      X += DuonaLarĝeco + Spaceto
      Return _
         String.Format(
            "h {0} v {1} l -{2} {3} h {2} v {1} h -{0} v -{1} l {2} -{3} h -{2} z m 0 {4} h {0} v {1} h -{0} z",
            DuonaLarĝeco, Dy, DuonaLarĝeco - Dx, DufojaAlteco - 2*Dy, DufojaAlteco\2 - Dy\2)
   End Function

   Private Function Perfekto() As String
      X += Dx + DuonaLarĝeco\2 + Spaceto*2
      Return String.Format("h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z m {5} 0 h {0} v {1} h -{0} z",
                           Dx, DufojaAlteco, Dx*2, DuonaLarĝeco\2, Alteco + Spaceto\2, DuonaLarĝeco\2 + Dx)
   End Function

   Private Function Progresivo() As String
      X += Dx*2 + DuonaLarĝeco\2 + Spaceto*2
      Return _
         String.Format(
            "h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
            Dx, DufojaAlteco, Dx*2, DuonaLarĝeco\2, Alteco + Spaceto\2)
   End Function

   Private Function Argumento1() As String
      X += Dx*2 + DuonaLarĝeco\2 + Spaceto
      Return _
         String.Format(
            "m {3} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z m {2} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z",
            Dx, DufojaAlteco, Dx*2, DuonaLarĝeco\2, Alteco + Spaceto\2)
   End Function

   Private Function Argumento2() As String
      X += Dx + DuonaLarĝeco + Spaceto
      Return _
         String.Format(
            "m {3} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
            Dx, DufojaAlteco, Dx*2, DuonaLarĝeco\2, Alteco + Spaceto\2)
   End Function

   Private Function Argumento3() As String
      X += Dx*2 + DuonaLarĝeco + Spaceto*2
      Return _
         String.Format(
            "m {3} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z
m {1} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
            Dx, Dx + Spaceto, Dx*2, DuonaLarĝeco\2, Alteco + Spaceto\2)
   End Function

   Private Function Estonteco() As String
      X += Spaceto*2 + DuonaLarĝeco\2 + Dx*2
      Return String.Format("h {0} v {1} h -{0} z m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
                           Dx, DufojaAlteco, Dx*2, DuonaLarĝeco\2, Alteco + Spaceto\2)
   End Function

   Private Function Igo() As String
      X += Dx*3 + DuonaLarĝeco + Spaceto
      Return _
         String.Format(
            "h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z m {5} 0 h {0} v {1} h -{0} z
m {5} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z",
            Dx, DufojaAlteco, Dx*2, DuonaLarĝeco\2, Alteco + Spaceto\2, DuonaLarĝeco\2 + Dx)
   End Function

   Private Function Translativo() As String
      X += Spaceto + DuonaLarĝeco\2 + Dx*2
      Return String.Format("h {0} v {1} h -{0} z m {5} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z",
                           Dx, DufojaAlteco, Dx*2, DuonaLarĝeco\2, Alteco + Spaceto\2, Dx + DuonaLarĝeco\2)
   End Function

   Private Function Ekzistado() As String
      X += DuonaLarĝeco + Spaceto
      Return _
         String.Format("h {0} v {1} l -{2} {3} l {2} {3} v {1} h -{0} v -{1} h {2} l -{2} -{3} l {2} -{3} h -{2} z",
                       DuonaLarĝeco, Dy, DuonaLarĝeco - Dx, DufojaAlteco\2 - Dy)
   End Function

   Private Function Imperativo() As String
      X += Dx*3 + DuonaLarĝeco + Spaceto
      Return _
         String.Format(
            "h {0} v {1} h -{0} z m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z
m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
            Dx, DufojaAlteco, Dx*2, DuonaLarĝeco\2, Alteco + Spaceto\2)
   End Function

   Private Function Invito() As String
      X += Dx*3 + DuonaLarĝeco + Spaceto
      Return _
         String.Format(
            "h {0} v {1} h -{0} z m 0 {5} h {0} v {1} h -{0} z m {2} -{5} h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z
m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
            Dx, Alteco, Dx*2, DuonaLarĝeco\2, Alteco + Spaceto\2, Alteco + Spaceto)
   End Function

   Private Function Volo1() As String
      X += DuonaLarĝeco + Spaceto*2 + Dx
      Return _
         String.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} {9} h {8} v {1} h -{8} z",
            DuonaLarĝeco, Alteco, DufojaAlteco, DufojaAlteco - Dy, DuonaLarĝeco - Dx, Alteco - Dy,
            DufojaAlteco - 2*Dy, Spaceto, Dx, Alteco + Spaceto)
   End Function

   Private Function Volo2() As String
      X += DuonaLarĝeco + Spaceto*3 + Dx*2
      Return _
         String.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} {9} h {8} v {1} h -{8} z m {10} 0 h {8} v {1} h -{8} z",
            DuonaLarĝeco, Alteco, DufojaAlteco, DufojaAlteco - Dy, DuonaLarĝeco - Dx, Alteco - Dy,
            DufojaAlteco - 2*Dy, Spaceto, Dx, Alteco + Spaceto, Dx + Spaceto)
   End Function

   Private Function Volo3() As String
      X += DuonaLarĝeco + Spaceto*4 + Dx*3
      Return _
         String.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} {9} h {8} v {1} h -{8} z
m {10} 0 h {8} v {1} h -{8} z m {10} 0 h {8} v {1} h -{8} z",
            DuonaLarĝeco, Alteco, DufojaAlteco, DufojaAlteco - Dy, DuonaLarĝeco - Dx, Alteco - Dy,
            DufojaAlteco - 2*Dy, Spaceto, Dx, Alteco + Spaceto, Dx + Spaceto)
   End Function

   Private Function Sola() As String
      X += DuonaLarĝeco + Spaceto
      Return _
         String.Format(
            "h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m -{2} {3} h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z",
            Dx, Alteco, DuonaLarĝeco - Dx, Alteco + Spaceto)
   End Function
   
   Private Function AktualaOkazo() As String
      X += Dx * 3 + Spaceto
      Return String.Format(
         "h {0} v {1} h {0} v -{1} h {0} v {2} h -{0} v -{1} h -{0} v {1} h -{0} z",
         Dx, (DufojaAlteco - Dy) / 2, DufojaAlteco)
   End Function
   
   Private Function Okazo() As String
      X += Dx * 3 + Spaceto
      Return String.Format(
         "h {0} v {1} h -{0} z m {2} {3} v {4} h {2} v -{4} z",
         Dx * 3, DufojaAlteco, Dx, Dy, DufojaAlteco - 2 * Dy)
   End Function
   
   Private Function FinitaOkazo() As String
      X += Dx * 4 + Spaceto * 2
      Return String.Format(
         "h {0} v {1} h -{0} z m {2} 0 h {3} v {1} h -{3} z m {0} {4} v {5} h {0} v -{5} z",
         Dx, DufojaAlteco, Dx + Spaceto, Dx * 3, Dy, DufojaAlteco - 2 * Dy)
   End Function

   Private Function Klaso() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("m {0} 0 h {1} l -{0} {2} l {0} {2} h -{1} l -{0} -{2} z",
                           DuonaLarĝeco - Dx, Dx, DufojaAlteco\2)
   End Function

   Private Function Difinito() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} l {1} -{0} h {3} l -{1} {0} h {1} v {2} h -{1} l {1} {0} h -{3} l -{1} -{0} z",
                           DufojaAlteco\2 - Dy\2, DuonaLarĝeco - Dx, Dy, Dx)
   End Function

   Private Function UnuNombro() As String
      X += DuonaLarĝeco + Spaceto*2 + Dx
      Return _
         String.Format(
            "m 0 {0} l {1} -{0} h {3} l -{1} {0} h {1} v {2} h -{1} l {1} {0} h -{3} l -{1} -{0} z m {4} -{0} h {3} v {5} h -{3} z",
            DufojaAlteco\2 - Dy\2, DuonaLarĝeco - Dx, Dy, Dx, DuonaLarĝeco + Spaceto, Alteco)
   End Function

   Private Function PluraNombro() As String
      X += DuonaLarĝeco + Spaceto*2 + Dx
      Return _
         String.Format(
            "m 0 {0} l {1} -{0} h {3} l -{1} {0} h {1} v {2} h -{1} l {1} {0} h -{3} l -{1} -{0} z m {4} -{0} h {3} v {5} h -{3} z
m 0 {6} h {3} v {5} h -{3} z",
            DufojaAlteco\2 - Dy\2, DuonaLarĝeco - Dx, Dy, Dx, DuonaLarĝeco + Spaceto, Alteco, Spaceto + Alteco)
   End Function

   Private Function Havaĵo() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {3} h -{2} v -{0} h -{1} z",
                           DufojaAlteco\2 - Dy\2, DuonaLarĝeco - Dx, Dx, DufojaAlteco)
   End Function

   Private Function AtributivoEstiMaldekstra() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {3} h -{2} v -{0} h -{1} v -{4} h {1} v -{4} h -{1} z",
                           (DufojaAlteco - 3*Dy)\2, DuonaLarĝeco - Dx, Dx, DufojaAlteco, Dy)
   End Function

   Private Function AtributivoEstiDekstra() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("h {2} v {0} h {1} v {4} h -{1} v {4} h {1} v {4} h -{1} v {0} h -{2} z",
                           (DufojaAlteco - 3*Dy)\2, DuonaLarĝeco - Dx, Dx, DufojaAlteco, Dy)
   End Function

   Private Function Ĝerundo() As String
      X += DuonaLarĝeco + Dx*2 + Spaceto
      Return _
         String.Format(
            "h {3} l {1} {0} h {3} l -{1} -{0} h {3} l {1} {0} v {2} l -{1} {0} h -{3} l {1} -{0} h -{3} l -{1} {0}
h -{3} l {1} -{0} h -{1} v -{2} h {1} z",
            DufojaAlteco\2 - Dy\2, DuonaLarĝeco - Dx, Dy, Dx)
   End Function

   Private Function Havado() As String
      X += DuonaLarĝeco + Dx*2 + Spaceto
      Return _
         String.Format(
            "m {1} 0 h {3} l -{1} {0} h {3} l {1} -{0} h {3} l -{1} {0} v {2} l {1} {0} h -{3}
l -{1} -{0} h -{3} l {1} {0} h -{3} l -{1} -{0} v -{2} z",
            DufojaAlteco\2 - Dy\2, DuonaLarĝeco - Dx, Dy, Dx)
   End Function

   Private Function MalplenaVerbo() As String
      X += DuonaLarĝeco + Spaceto*2
      Return _
         String.Format(
            "h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{5} {6} h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z",
            0, Dx, DuonaLarĝeco\2, Alteco, DuonaLarĝeco + Dx, DuonaLarĝeco + Dx, Alteco + Spaceto)
   End Function

   Private Function OblikaNetransitiva() As String
      X += DuonaLarĝeco + Spaceto*2
      Return _
         String.Format(
            "h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{4} {5} h {4} v {7} h -{8} v {6} h -{1} v -{6} h -{8} z",
            0, Dx, DuonaLarĝeco\2, Alteco, DuonaLarĝeco + Dx, Alteco + Spaceto, Alteco - Dy, Dy,
            (DuonaLarĝeco + Dx)\2 - Dx\2)
   End Function

   Private Function OblikaTransitiva() As String
      X += DuonaLarĝeco + Spaceto*2
      Return _
         String.Format(
            "h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{4} {5} h {4} v {7} h -{8} v {6} h {8} v {7} h -{4} v -{7} h {8} v -{6} h -{8} z",
            0, Dx, DuonaLarĝeco\2, Alteco, DuonaLarĝeco + Dx, Alteco + Spaceto, Alteco - Dy*2, Dy,
            DuonaLarĝeco\2 - Dx\2 + Spaceto\2)
   End Function

   Private Function NedirektaNetransitiva() As String
      X += DuonaLarĝeco + Spaceto*2
      Return _
         String.Format(
            "h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{2} {5} v {6} h {8} v {7} h -{4} v -{7} h {2} v -{6} z",
            0, Dx, DuonaLarĝeco\2, Alteco, DuonaLarĝeco + Dx, Alteco + Spaceto, Alteco - Dy, Dy,
            DuonaLarĝeco\2 + Spaceto\2 - Dx\2)
   End Function

   Private Function ModifantoMaldekstra() As String
      X += DuonaLarĝeco + Spaceto
      Return _
         String.Format("h {0} v {1} h -{4} v -{3} h -{2} z", DuonaLarĝeco, DufojaAlteco, DuonaLarĝeco - Dx,
                       DufojaAlteco - Dy, Dx)
   End Function

   Private Function ModifantoDekstra() As String
      X += DuonaLarĝeco + Spaceto
      Return _
         String.Format("h {0} v {1} h -{2} v {3} h -{4} z", DuonaLarĝeco, Dy, DuonaLarĝeco - Dx, DufojaAlteco - Dy,
                       Dx)
   End Function

   Private Function NombrigeblaEcoDekstra() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{2} v {3} h {2} v {1} h -{0} z",
                           DuonaLarĝeco, Dy, DuonaLarĝeco - Dx, Alteco + Spaceto\2 - Dy*3\2)
   End Function

   Private Function NombrigeblaEcoMaldekstra() As String
      X += DuonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{0} v -{2} h {3} v -{4} h -{3} v {2} h {3} v -{4} h -{3} z",
                           DuonaLarĝeco, DufojaAlteco, Dy, DuonaLarĝeco - Dx, Alteco + Spaceto\2 - Dy\2)
   End Function

   Private Function MankaNominativo() As String
      X += Dx + Spaceto + DuonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{0} z m {2} 0 h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
                           Dx, DufojaAlteco\2, Dx + Spaceto, DuonaLarĝeco - Dx)
   End Function

   Private Function Ujo1Unue() As String
      X += Dx + Spaceto + DuonaLarĝeco + Spaceto
      Return String.Format("m 0 {4} h {0} v {1} h -{0} z m {2} -{4} h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
                           Dx, DufojaAlteco\2, Dx + Spaceto, DuonaLarĝeco - Dx, Alteco + Spaceto\2)
   End Function

   Private Function Ujo2Unue() As String
      X += (Dx + Spaceto)*2 + DuonaLarĝeco + Spaceto
      Return _
         String.Format(
            "m 0 {4} h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m {2} -{4}
h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
            Dx, DufojaAlteco\2, Dx + Spaceto, DuonaLarĝeco - Dx, Alteco + Spaceto\2)
   End Function

   Private Function Ujo3Unue() As String
      X += (Dx + Spaceto)*3 + DuonaLarĝeco + Spaceto
      Return _
         String.Format(
            "m 0 {4} h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m {2} -{4}
h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
            Dx, DufojaAlteco\2, Dx + Spaceto, DuonaLarĝeco - Dx, Alteco + Spaceto\2)
   End Function

   Public Function Vico() As String
      DokumentoLarĝeco = Math.Max(DokumentoLarĝeco, X + Spaco)
      X = Spaco
      Y += DufojaAlteco + Spaco
      Return ""
   End Function
   
   Private Function Cifero() As String
      X += Dx + Spaceto
      Return String.Format("h {0} v {1} h -{0} z", Dx, DufojaAlteco)
   End Function

   Private Function M(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h {2} v {3} h -{4} v -{5}",
                           Dx, aktualaAlteco - Dy, aktualaLarĝeco - Dx, Dy, aktualaLarĝeco, aktualaAlteco)
   End Function

   Private Function P(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} v -{5}",
                           aktualaLarĝeco, Dy, aktualaLarĝeco - Dx, aktualaAlteco - Dy, Dx, aktualaAlteco)
   End Function

   Private Function Pl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{6} v -{2} h -{5} v {2} h -{3} z m {9} {7} v {8} h {1} v -{8} z",
                           aktualaLarĝeco, aktualaLarĝeco\5, aktualaAlteco - Dy, Dx, aktualaAlteco,
                           aktualaLarĝeco - (aktualaLarĝeco\5 + Dx*3),
                           (aktualaLarĝeco\5 + Dx*2), Dy, aktualaAlteco - 2*Dy,
                           aktualaAlteco - aktualaAlteco\5 - Dx*3)
   End Function

   Private Function Pr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim rSpaco = aktualaLarĝeco\5
      Return _
         String.Format(
            "v {4} h {3} v -{2} h {1} v -{5} z m {0} 0 m 0 {5} l -{6} {2} h -{3} l {6} -{2} z m -{7} 0 l {6} {2} h -{3} l -{6} -{2} z",
            aktualaLarĝeco, aktualaLarĝeco - Dx, aktualaAlteco - Dy, Dx, aktualaAlteco,
            Dy, rSpaco\2 + Dx\2, rSpaco + Dx)
   End Function

   Private Function B(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{5} v {2} h -{3} v -{2} h -{1} v {2} h -{3} z",
                           aktualaLarĝeco, Dx*2, aktualaAlteco - Dy, Dx, Dy, aktualaLarĝeco - Dx*4)
   End Function

   Private Function Bl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return _
         String.Format(
            "h {0} v {4} h -{6} v -{2} h -{5} v {2} h -{3} v -{2} h -{1} v {2} h -{3} z m {9} {7} v {8} h {1} v -{8} z",
            aktualaLarĝeco, aktualaLarĝeco\5, aktualaAlteco - Dy, Dx, aktualaAlteco,
            aktualaLarĝeco - 2*(aktualaLarĝeco\5 + Dx*2),
            (aktualaLarĝeco\5 + Dx*2), Dy, aktualaAlteco - 2*Dy, aktualaAlteco - aktualaAlteco\5 - Dx*3)
   End Function

   Private Function Br(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim rSpaco = aktualaLarĝeco\5
      Return _
         String.Format(
            "v {4} h {3} v -{2} h {8} v {2} h {3} v -{2} h {1} v -{5} z m {0} 0 m 0 {5}
l -{6} {2} h -{3} l {6} -{2} z m -{7} 0 l {6} {2} h -{3} l -{6} -{2} z",
            aktualaLarĝeco, aktualaLarĝeco - (aktualaLarĝeco\5 + 2*Dx),
            aktualaAlteco - Dy, Dx, aktualaAlteco,
            Dy, rSpaco\2 + Dx\2, rSpaco + Dx, aktualaLarĝeco\5)
   End Function

   Private Function V(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m 0 {1} l {2} {3} h {4} l -{2} -{3} z",
                           aktualaLarĝeco, Dy,
                           If(aktualaLarĝeco < Larĝeco, aktualaLarĝeco - Dx, aktualaLarĝeco\2 - Dx\2),
                           aktualaAlteco - Dy, Dx)
   End Function

   Private Function K(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{3} v -{2} h -{1} z",
                           aktualaLarĝeco, aktualaLarĝeco - Dx, aktualaAlteco - Dy, Dx, aktualaAlteco)
   End Function

   Private Function Kl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim l = aktualaLarĝeco\5
      Dim lLarĝeco = l + 2*Dx
      Return String.Format("h {0} v {1} h -{2} v -{3} h -{4} v {3} h -{5} z m {2} {6} v {7} h {8} v -{7} z",
                           aktualaLarĝeco, aktualaAlteco, Dx, aktualaAlteco - Dy, aktualaLarĝeco - lLarĝeco - Dx,
                           lLarĝeco, Dy,
                           aktualaAlteco - Dy*2, l)
   End Function

   Private Function Kr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim rSpaco = aktualaLarĝeco\5
      Return _
         String.Format(
            "h {0} v {4} h -{3} v -{2} h -{1} z m 0 {5} l {6} {2} h {3} l -{6} -{2} z m {7} 0 l -{6} {2} h {3} l {6} -{2} z",
            aktualaLarĝeco, aktualaLarĝeco - Dx, aktualaAlteco - Dy, Dx, aktualaAlteco,
            Dy, rSpaco\2 + Dx\2, rSpaco + Dx)
   End Function

   Private Function G(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{3} v -{2} h -{1} v {2} h -{3} v -{2} h -{5} z",
                           aktualaLarĝeco, Dx*2, aktualaAlteco - Dy, Dx, aktualaAlteco, aktualaLarĝeco - Dx*4)
   End Function

   Private Function Gr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim rSpaco = aktualaLarĝeco\5
      Return _
         String.Format(
            "h {0} v {4} h -{3} v -{2} h -{1} v {2} h -{3} v -{2} h -{8} z m 0 {5} l {6} {2} h {3} l -{6} -{2} z m {7} 0
l -{6} {2} h {3} l {6} -{2} z",
            aktualaLarĝeco, rSpaco, aktualaAlteco - Dy, Dx, aktualaAlteco,
            Dy, rSpaco\2 + Dx\2, rSpaco + Dx, aktualaLarĝeco - rSpaco - 2*Dx)
   End Function

   Private Function Gl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return _
         String.Format(
            "h {0} v {4} h -{3} v -{2} h -{1} v {2} h -{3} v -{2} h -{5} v {2} h -{6} z m {3} {7} v {8} h {1} v -{8} z",
            aktualaLarĝeco, aktualaLarĝeco\5, aktualaAlteco - Dy, Dx, aktualaAlteco,
            aktualaLarĝeco - 2*(aktualaLarĝeco\5 + Dx*2),
            (aktualaLarĝeco\5 + Dx*2), Dy, aktualaAlteco - 2*Dy)
   End Function

   Private Function E(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h {2} v {3} h -{2} v {1} h -{0} z",
                           Dx, aktualaAlteco\2 - Dy\2, aktualaLarĝeco - Dx, Dy)
   End Function

   Private Function I(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} z",
                           aktualaLarĝeco, Dy, aktualaLarĝeco - Dx, aktualaAlteco - Dy, Dx)
   End Function

   Private Function O(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("m 0 {1} h {2} v -{1} h {0} v {3} h -{0} v -{1} h -{2} z",
                           Dx, aktualaAlteco\2 - Dy\2, aktualaLarĝeco - Dx, aktualaAlteco)
   End Function

   Private Function AA(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("m 0 {3} h {2} v -{3} h {4} v {5} h -{0} z",
                           aktualaLarĝeco, Dy, aktualaLarĝeco - Dx, aktualaAlteco - Dy, Dx, aktualaAlteco)
   End Function

   Private Function N(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {0} h {1} v {3} h -{4}",
                           aktualaAlteco - Dy,
                           aktualaLarĝeco\2 - Dx\2,
                           Dx,
                           Dy,
                           aktualaLarĝeco)
   End Function

   Private Function H(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m 0 {2} h {0} v {1} h -{0} z",
                           aktualaLarĝeco,
                           Dy,
                           aktualaAlteco - Dy)
   End Function

   Private Function T(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} v -{3} h -{2} z",
                           aktualaLarĝeco,
                           Dy,
                           aktualaLarĝeco\2 - Dx\2,
                           aktualaAlteco - Dy,
                           Dx)
   End Function

   Private Function Tl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} v -{3} h -{2} z m {5} {1} v {6} h {7} v -{6} z",
                           aktualaLarĝeco,
                           Dy,
                           aktualaLarĝeco\2 - (aktualaLarĝeco\5 + 2*Dx)\2,
                           aktualaAlteco - Dy,
                           aktualaLarĝeco\5 + 2*Dx,
                           aktualaLarĝeco\2 - (aktualaLarĝeco\5 + 2*Dx)\2 + Dx, aktualaAlteco - 2*Dy,
                           aktualaLarĝeco\5)
   End Function

   Private Function Tr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m {2} 0 h {3} l {5} {4} h -{3} z m {6} 0 h {3} l -{5} {4} h -{3} z",
                           aktualaLarĝeco,
                           Dy,
                           aktualaLarĝeco\2 - (aktualaLarĝeco\5 + 2*Dx)\2,
                           Dx,
                           aktualaAlteco,
                           aktualaLarĝeco\10 + Dx\2,
                           aktualaLarĝeco\5 + Dx)
   End Function

   Private Function D(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {5} h -{3} v -{5} h -{4} v {5} h -{3} v -{5} h -{2} z",
                           aktualaLarĝeco,
                           Dy, aktualaLarĝeco\2 - 2*Dx, Dx, 2*Dx, aktualaAlteco - Dy)
   End Function

   Private Function Dl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim l = aktualaLarĝeco\5
      Dim lLarĝeco = aktualaLarĝeco\5 + 2*Dx
      Return _
         String.Format(
            "h {0} v {1} h -{2} v -{3} h -{4} v {3} h -{2} z m {5} {6} v {7} h {8} v -{7} z m {9} 0 v {7} h {8} v -{7} z",
            aktualaLarĝeco, aktualaAlteco, lLarĝeco, aktualaAlteco - Dy, aktualaLarĝeco - 2*lLarĝeco, Dx, Dy,
            aktualaAlteco - 2*Dy, l, l + Dx*2 + aktualaLarĝeco - 2*lLarĝeco)
   End Function

   Private Function Dr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return _
         String.Format(
            "h {0} v {1} h -{0} z m 0 {1} l {2} {3} h {4} l -{2} -{3} z m {2} 0 v {3} h {4} v -{3} z m {5} 0
h {4} v {3} h -{4} z m 0 {3} l {2} -{3} h {4} l -{2} {3} z",
            aktualaLarĝeco, Dy, aktualaLarĝeco\2 - Dx*2, aktualaAlteco - Dy, Dx, Dx*3)
   End Function

   Private Function S(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return _
         String.Format("h {0} v {1} h -{0} z m 0 {2} l {3} -{2} h {4} l -{3} {2} z m {3} -{2} h {4} l {3} {2} h -{4}",
                       aktualaLarĝeco, Dy, aktualaAlteco, aktualaLarĝeco\2 - Dx\2, Dx)
   End Function

   Private Function R(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return _
         String.Format("h {0} v {1} h -{0} z m 0 {1} l {2} {3} h {4} l -{2} -{3} z m {0} 0 h -{4} l -{2} {3} h {4} z",
                       aktualaLarĝeco, Dy, aktualaLarĝeco\2 - Dx\2,
                       aktualaAlteco - Dy, Dx)
   End Function

   Private Function L(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m {2} {3} v {4} h {5} v -{4} z",
                           aktualaLarĝeco, aktualaAlteco, Dx, Dy, aktualaAlteco - 2*Dy, aktualaLarĝeco - 2*Dx)
   End Function

   Private Function J(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{0} v -{1} h {2} v -{3} h -{2} z",
                           aktualaLarĝeco,
                           Dy,
                           aktualaLarĝeco\2 - Dx\2,
                           aktualaAlteco - Dy*2)
   End Function

   Private Function W(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return _
         String.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} v -{3} a {4} {5} 0 0 1 0 -{6} z a {0} {1} 0 0 1 0 {2} v -{3} a {4} {5} 0 0 0 0 -{6} z",
            aktualaLarĝeco\2, aktualaAlteco\2, aktualaAlteco, Dy, aktualaLarĝeco\2 - Dx, aktualaAlteco\2 - Dy,
            aktualaAlteco - Dy*2)
   End Function
   
   Public Sub New(elirejo As String, alteco As Integer, larĝeco As Integer, dx As Integer, dy As Integer,
                  Optional spaco As Integer = 8)
      MyBase.New(elirejo, alteco, larĝeco, dx, dy, spaco)
   End Sub
End Class
