Imports System.Xml

Public Class SVGDesegnilo
   Private ReadOnly xmlSkribilo As XmlWriter
   Private ReadOnly alteco, larĝeco As Integer
   Private ReadOnly dufojaAlteco, duonaLarĝeco As Integer
   Private x, y As Double
   Private dokumentoLarĝeco As Integer
   Private ReadOnly literoDesegniloj As Dictionary(Of String, LiteroDesegnilo) =
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
   Private ReadOnly finaĵoDesegniloj As Dictionary(Of String, FinaĵoDesegnilo) =
      New Dictionary(Of String, FinaĵoDesegnilo) From {
      {"[", AddressOf NomoKomenco},
      {"]", AddressOf NomoFino},
      {"lokokupilo", AddressOf Lokokupilo},
      {"klaso", AddressOf Klaso},
      {"eco>", AddressOf EcoDekstra},
      {"eco<", AddressOf EcoMaldekstra},
      {"rekordo<", AddressOf RekordoMaldekstra},
      {"rekordo>", AddressOf RekordoDekstra},
      {"pridiranto", AddressOf Pridiranto},
      {"atributivoEsti<", AddressOf AtributivoEstiMaldekstra},
      {"atributivoEsti>", AddressOf AtributivoEstiDekstra},
      {"predikativoEsti", AddressOf PredikativoEsti},
      {"vico", AddressOf Vico},
      {"netransitiva1", AddressOf Netransitiva1},
      {"netransitiva2", AddressOf Netransitiva2},
      {"transitiva2", AddressOf Transitiva2},
      {"transitiva3", AddressOf Transitiva3},
      {"imperativo", AddressOf Imperativo},
      {"invito", AddressOf Invito},
      {"nekonitaNombro", AddressOf NekonitaNombro},
      {"unuNombro", AddressOf UnuNombro},
      {"pluraNombro", AddressOf PluraNombro},
      {"havaĵo", AddressOf Havaĵo},
      {"malplenaVerbo", AddressOf MalplenaVerbo},
      {"partaTransitiva1", AddressOf PartaTransitiva1},
      {"partaTransitiva2", AddressOf PartaTransitiva2},
      {"partaNetransitiva", AddressOf PartaNetransitiva},
      {"progresivo", AddressOf Progresivo},
      {"perfekto", AddressOf Perfekto},
      {"volo1", AddressOf Volo1},
      {"volo2", AddressOf Volo2},
      {"volo3", AddressOf Volo3},
      {"sola", AddressOf Sola},
      {"estonteco", AddressOf Estonteco},
      {"modifanto<", AddressOf ModifantoMaldekstra},
      {"modifanto>", AddressOf ModifantoDekstra},
      {"nombrigeblaEco>", AddressOf NombrigeblaEcoDekstra},
      {"nombrigeblaEco<", AddressOf NombrigeblaEcoMaldekstra},
      {"mankaNominativo", AddressOf MankaNominativo},
      {"ĝerundo", AddressOf Ĝerundo},
      {"ekzistado", AddressOf Ekzistado},
      {"havado", AddressOf Havado},
      {"aganto", AddressOf Aganto},
      {"patiento", AddressOf Patiento},
      {"translativo", AddressOf Translativo},
      {"partaNominativo", AddressOf PartaNominativo},
      {"partaAkuzativo", AddressOf PartaAkuzativo},
      {"partaDativo", AddressOf PartaDativo},
      {"igo", AddressOf Igo},
      {"etigo", AddressOf Etigo}
   }
   Private ReadOnly vojoj As List(Of String) = New List(Of String)
   Private ReadOnly dx As Integer
   Private ReadOnly dy As Integer
   Private ReadOnly spaco As Integer
   Private ReadOnly Property Spaceto As Integer
      Get
         Return spaco / 2
      End Get
   End Property

   Delegate Function LiteroDesegnilo(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
   Delegate Function FinaĵoDesegnilo() As String

   Public Sub New(elirejo As String, alteco As Integer, larĝeco As Integer, dx As Integer, dy As Integer, Optional spaco As Integer = 8)
      xmlSkribilo = XmlWriter.Create(elirejo)
      Me.alteco = alteco \ 2
      Me.larĝeco = larĝeco + Spaceto
      Me.dx = dx
      Me.dy = dy
      Me.spaco = spaco
      dufojaAlteco = alteco + Spaceto
      duonaLarĝeco = larĝeco \ 2
      dokumentoLarĝeco = 0
      x = 10
      y = 10
   End Sub

   Public Sub DesegniSilabon(silabo As String)
      If silabo.Length = 1 Then
         AldoniVojon(literoDesegniloj(silabo)(larĝeco, dufojaAlteco), x, y)
         x += larĝeco + Spaceto
      ElseIf silabo.Length = 2 Then
         If ĈuVokalo(silabo.Chars(0)) Then 'VC
            AldoniVojon(literoDesegniloj(silabo.Substring(0, 1))(duonaLarĝeco, dufojaAlteco), x, y)
            x += larĝeco \ 2 + Spaceto
            AldoniVojon(literoDesegniloj(silabo.Substring(1, 1))(duonaLarĝeco, dufojaAlteco), x, y)
            x += larĝeco \ 2 + Spaceto
         Else 'CV
            AldoniVojon(literoDesegniloj(silabo.Substring(0, 1))(larĝeco, alteco), x, y)
            Dim antaŭY = y
            y += alteco + Spaceto
            AldoniVojon(literoDesegniloj(silabo.Substring(1, 1))(larĝeco, alteco), x, y)
            x += larĝeco + Spaceto
            y = antaŭY
         End If
      ElseIf silabo.Length = 3 Then
         If silabo.Chars(1) = "r"c Or silabo.Chars(1) = "l"c Then 'CCV
            AldoniVojon(literoDesegniloj(silabo.Substring(0, 2))(larĝeco, alteco), x, y)
            Dim antaŭY = y
            y += alteco + Spaceto
            AldoniVojon(literoDesegniloj(silabo.Substring(2, 1))(larĝeco, alteco), x, y)
            x += larĝeco + Spaceto
            y = antaŭY
         Else 'CVC
            AldoniVojon(literoDesegniloj(silabo.Substring(0, 1))(larĝeco, alteco), x, y)
            Dim antaŭY = y
            y += alteco + Spaceto
            AldoniVojon(literoDesegniloj(silabo.Substring(1, 1))(duonaLarĝeco, alteco), x, y)
            x += larĝeco \ 2 + Spaceto
            AldoniVojon(literoDesegniloj(silabo.Substring(2, 1))(duonaLarĝeco, alteco), x, y)
            x += larĝeco \ 2 + Spaceto
            y = antaŭY
         End If
      ElseIf silabo.Length = 4 Then 'CCVC
         AldoniVojon(literoDesegniloj(silabo.Substring(0, 2))(larĝeco, alteco), x, y)
         Dim antaŭY = y
         y += alteco + Spaceto
         AldoniVojon(literoDesegniloj(silabo.Substring(2, 1))(duonaLarĝeco, alteco), x, y)
         x += larĝeco \ 2 + Spaceto
         AldoniVojon(literoDesegniloj(silabo.Substring(3, 1))(duonaLarĝeco, alteco), x, y)
         x += larĝeco \ 2 + Spaceto
         y = antaŭY
      Else
         Throw New Exception($"Nevalida silabo: {silabo}")
      End If
   End Sub

   Public Function NomoKomenco() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{0} z",
                           duonaLarĝeco, dy, duonaLarĝeco - dx, dufojaAlteco - dy * 2)
   End Function

   Public Function NomoFino() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{0} v -{2} h {3} v -{4} h -{3} z",
                           duonaLarĝeco, dufojaAlteco, dy, duonaLarĝeco - dx, dufojaAlteco - dy * 2)
   End Function

   Public Function Lokokupilo() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {0} h {3} v -{0} h {2} v {4} h -{5} z",
                           dufojaAlteco - dy, duonaLarĝeco \ 2 - dx \ 2, dx, duonaLarĝeco \ 2 - dx * 3 \ 2, dufojaAlteco,
                           duonaLarĝeco)
   End Function

   Private Function EcoDekstra() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m 0 {7} h -{0} v {8} h {0} z",
                           duonaLarĝeco, alteco, dufojaAlteco, dufojaAlteco - dy, duonaLarĝeco - dx, alteco - dy,
                           dufojaAlteco - 2 * dy,
                           alteco - dy \ 2, dy)
   End Function

   Private Function EcoMaldekstra() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("a {0} {1} 0 0 1 0 {2} z v {3} a {4} {5} 0 0 0 0 -{6} z m 0 {7} h {0} v {8} h -{0} z",
                           duonaLarĝeco, alteco, dufojaAlteco, dufojaAlteco - dy, duonaLarĝeco - dx, alteco - dy,
                           dufojaAlteco - 2 * dy,
                           alteco - dy \ 2, dy)
   End Function

   Private Function RekordoMaldekstra() As String
      x += duonaLarĝeco + Spaceto * 2 + dx
      Return String.Format("a {0} {1} 0 0 1 0 {2} z v {3} a {4} {5} 0 0 0 0 -{6} z m {7} 0 h {8} v {1} h -{8} z m 0 {9} h {8} v {1} h -{8} z",
                           duonaLarĝeco, alteco, dufojaAlteco, dufojaAlteco - dy, duonaLarĝeco - dx, alteco - dy,
                           dufojaAlteco - 2 * dy, duonaLarĝeco + Spaceto, dx, alteco + Spaceto)
   End Function

   Private Function RekordoDekstra() As String
      x += duonaLarĝeco + Spaceto * 2 + dx
      Return String.Format("m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} 0 h {8} v {1} h -{8} z m 0 {9} h {8} v {1} h -{8} z",
                           duonaLarĝeco, alteco, dufojaAlteco, dufojaAlteco - dy, duonaLarĝeco - dx, alteco - dy,
                           dufojaAlteco - 2 * dy, Spaceto, dx, alteco + Spaceto)
   End Function

   Private Function PredikativoEsti() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("m 0 {3} h {5} v {6} h -{5} z m {1} -{3} h {2} v {4} h -{2} z m {7} 0  h {2} v {4} h -{2} z",
                           dufojaAlteco - dy, duonaLarĝeco \ 2 - dx \ 2, dx, dufojaAlteco \ 2 - dy \ 2, dufojaAlteco,
                           duonaLarĝeco, dy, duonaLarĝeco \ 2)
   End Function

   Private Function Pridiranto() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("m 0 {3} h {0} v {1} h -{0} z l {2} -{3} h {4} l -{2} {3} z m {2} -{3} h {4} l {2} {3} h -{4} z",
                           duonaLarĝeco, dy, duonaLarĝeco \ 2 - dx \ 2, dufojaAlteco - dy, dx)
   End Function

   Private Function Etigo() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("m 0 {5} h {0} v {1} h -{0} z l {2} -{3} h {4} l -{2} {3} z m {2} -{3} h {4} l {2} {3} h -{4} z",
                           duonaLarĝeco, dy, duonaLarĝeco \ 2 - dx \ 2, dufojaAlteco \ 2 - dy, dx, dufojaAlteco - dy)
   End Function

   Private Function Netransitiva1() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {0} h {1} v {3} h -{1} v {0} h -{2} v -{0} h -{1} z",
                           dufojaAlteco \ 2 - dy \ 2, duonaLarĝeco \ 2 - dx \ 2, dx, dy)
   End Function

   Private Function Netransitiva2() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{0} v -{1} h {2} v -{3} h -{2} z",
                           duonaLarĝeco, dy, duonaLarĝeco \ 2 - dx \ 2, dufojaAlteco - 2 * dy)
   End Function

   Private Function Transitiva2() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} l -{2} {3} h {2} v {1} h -{0} v -{1} l {2} -{3} h -{2} z",
                           duonaLarĝeco, dy, duonaLarĝeco - dx, dufojaAlteco - 2 * dy)
   End Function

   Private Function Transitiva3() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} l -{2} {3} h {2} v {1} h -{0} v -{1} l {2} -{3} h -{2} z m 0 {4} h {0} v {1} h -{0} z",
                           duonaLarĝeco, dy, duonaLarĝeco - dx, dufojaAlteco - 2 * dy, dufojaAlteco \ 2 - dy \ 2)
   End Function

   Private Function Perfekto() As String
      x += dx + duonaLarĝeco \ 2 + Spaceto * 2
      Return String.Format("h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z m {5} 0 h {0} v {1} h -{0} z",
                           dx, dufojaAlteco, dx * 2, duonaLarĝeco \ 2, alteco + Spaceto \ 2, duonaLarĝeco \ 2 + dx)
   End Function

   Private Function Progresivo() As String
      x += dx * 2 + duonaLarĝeco \ 2 + Spaceto
      Return String.Format("h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
                           dx, dufojaAlteco, dx * 2, duonaLarĝeco \ 2, alteco + Spaceto \ 2)
   End Function

   Private Function Aganto() As String
      x += dx * 2 + duonaLarĝeco \ 2 + Spaceto
      Return String.Format("m {3} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z m {2} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z",
                           dx, dufojaAlteco, dx * 2, duonaLarĝeco \ 2, alteco + Spaceto \ 2)
   End Function

   Private Function Patiento() As String
      x += dx + duonaLarĝeco + Spaceto
      Return String.Format("m {3} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
                           dx, dufojaAlteco, dx * 2, duonaLarĝeco \ 2, alteco + Spaceto \ 2)
   End Function

   Private Function Estonteco() As String
      x += Spaceto * 2 + duonaLarĝeco \ 2 + dx * 2
      Return String.Format("h {0} v {1} h -{0} z m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
                           dx, dufojaAlteco, dx * 2, duonaLarĝeco \ 2, alteco + Spaceto \ 2)
   End Function

   Private Function Igo() As String
      x += dx * 3 + duonaLarĝeco + Spaceto
      Return String.Format("h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z m {5} 0 h {0} v {1} h -{0} z
m {5} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z",
                           dx, dufojaAlteco, dx * 2, duonaLarĝeco \ 2, alteco + Spaceto \ 2, duonaLarĝeco \ 2 + dx)
   End Function

   Private Function Translativo() As String
      x += Spaceto + duonaLarĝeco \ 2 + dx * 2
      Return String.Format("h {0} v {1} h -{0} z m {5} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z",
                           dx, dufojaAlteco, dx * 2, duonaLarĝeco \ 2, alteco + Spaceto \ 2, dx + duonaLarĝeco \ 2)
   End Function

   Private Function Ekzistado() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} l -{2} {3} l {2} {3} v {1} h -{0} v -{1} h {2} l -{2} -{3} l {2} -{3} h -{2} z",
                           duonaLarĝeco, dy, duonaLarĝeco - dx, dufojaAlteco \ 2 - dy)
   End Function

   Private Function Imperativo() As String
      x += dx * 3 + duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{0} z m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z
m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
                           dx, dufojaAlteco, dx * 2, duonaLarĝeco \ 2, alteco + Spaceto \ 2)
   End Function

   Private Function Invito() As String
      x += dx * 3 + duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{0} z m 0 {5} h {0} v {1} h -{0} z m {2} -{5} h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z
m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
                           dx, alteco, dx * 2, duonaLarĝeco \ 2, alteco + Spaceto \ 2, alteco + Spaceto)
   End Function

   Private Function Volo1() As String
      x += duonaLarĝeco + Spaceto * 2 + dx
      Return String.Format("m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} {9} h {8} v {1} h -{8} z",
                           duonaLarĝeco, alteco, dufojaAlteco, dufojaAlteco - dy, duonaLarĝeco - dx, alteco - dy,
                           dufojaAlteco - 2 * dy, Spaceto, dx, alteco + Spaceto)
   End Function

   Private Function Volo2() As String
      x += duonaLarĝeco + Spaceto * 3 + dx * 2
      Return String.Format("m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} {9} h {8} v {1} h -{8} z m {10} 0 h {8} v {1} h -{8} z",
                           duonaLarĝeco, alteco, dufojaAlteco, dufojaAlteco - dy, duonaLarĝeco - dx, alteco - dy,
                           dufojaAlteco - 2 * dy, Spaceto, dx, alteco + Spaceto, dx + Spaceto)
   End Function

   Private Function Volo3() As String
      x += duonaLarĝeco + Spaceto * 4 + dx * 3
      Return String.Format("m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} {9} h {8} v {1} h -{8} z
m {10} 0 h {8} v {1} h -{8} z m {10} 0 h {8} v {1} h -{8} z",
                           duonaLarĝeco, alteco, dufojaAlteco, dufojaAlteco - dy, duonaLarĝeco - dx, alteco - dy,
                           dufojaAlteco - 2 * dy, Spaceto, dx, alteco + Spaceto, dx + Spaceto)
   End Function

   Private Function Sola() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m -{2} {3} h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z",
                           dx, alteco, duonaLarĝeco - dx, alteco + Spaceto)
   End Function

   Private Function Klaso() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("m {0} 0 h {1} l -{0} {2} l {0} {2} h -{1} l -{0} -{2} z",
                           duonaLarĝeco - dx, dx, dufojaAlteco \ 2)
   End Function

   Private Function NekonitaNombro() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} l {1} -{0} h {3} l -{1} {0} h {1} v {2} h -{1} l {1} {0} h -{3} l -{1} -{0} z",
                           dufojaAlteco \ 2 - dy \ 2, duonaLarĝeco - dx, dy, dx)
   End Function

   Private Function UnuNombro() As String
      x += duonaLarĝeco + Spaceto * 2 + dx
      Return String.Format("m 0 {0} l {1} -{0} h {3} l -{1} {0} h {1} v {2} h -{1} l {1} {0} h -{3} l -{1} -{0} z m {4} -{0} h {3} v {5} h -{3} z",
                           dufojaAlteco \ 2 - dy \ 2, duonaLarĝeco - dx, dy, dx, duonaLarĝeco + Spaceto, alteco)
   End Function

   Private Function PluraNombro() As String
      x += duonaLarĝeco + Spaceto * 2 + dx
      Return String.Format("m 0 {0} l {1} -{0} h {3} l -{1} {0} h {1} v {2} h -{1} l {1} {0} h -{3} l -{1} -{0} z m {4} -{0} h {3} v {5} h -{3} z
m 0 {6} h {3} v {5} h -{3} z",
                           dufojaAlteco \ 2 - dy \ 2, duonaLarĝeco - dx, dy, dx, duonaLarĝeco + Spaceto, alteco, Spaceto + alteco)
   End Function

   Private Function Havaĵo() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {3} h -{2} v -{0} h -{1} z",
                           dufojaAlteco \ 2 - dy \ 2, duonaLarĝeco - dx, dx, dufojaAlteco)
   End Function

   Private Function AtributivoEstiMaldekstra() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {3} h -{2} v -{0} h -{1} v -{4} h {1} v -{4} h -{1} z",
                           (dufojaAlteco - 3 * dy) \ 2, duonaLarĝeco - dx, dx, dufojaAlteco, dy)
   End Function

   Private Function AtributivoEstiDekstra() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("h {2} v {0} h {1} v {4} h -{1} v {4} h {1} v {4} h -{1} v {0} h -{2} z",
                           (dufojaAlteco - 3 * dy) \ 2, duonaLarĝeco - dx, dx, dufojaAlteco, dy)
   End Function

   Private Function Ĝerundo() As String
      x += duonaLarĝeco + dx * 2 + Spaceto
      Return String.Format("h {3} l {1} {0} h {3} l -{1} -{0} h {3} l {1} {0} v {2} l -{1} {0} h -{3} l {1} -{0} h -{3} l -{1} {0}
h -{3} l {1} -{0} h -{1} v -{2} h {1} z",
                           dufojaAlteco \ 2 - dy \ 2, duonaLarĝeco - dx, dy, dx)
   End Function

   Private Function Havado() As String
      x += duonaLarĝeco + dx * 2 + Spaceto
      Return String.Format("m {1} 0 h {3} l -{1} {0} h {3} l {1} -{0} h {3} l -{1} {0} v {2} l {1} {0} h -{3}
l -{1} -{0} h -{3} l {1} {0} h -{3} l -{1} -{0} v -{2} z",
                           dufojaAlteco \ 2 - dy \ 2, duonaLarĝeco - dx, dy, dx)
   End Function

   Private Function MalplenaVerbo() As String
      x += duonaLarĝeco + Spaceto * 2
      Return String.Format("h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{5} {6} h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z",
                           0, dx, duonaLarĝeco \ 2, alteco, duonaLarĝeco + dx, duonaLarĝeco + dx, alteco + Spaceto)
   End Function

   Private Function PartaTransitiva1() As String
      x += duonaLarĝeco + Spaceto * 2
      Return String.Format("h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{4} {5} h {4} v {7} h -{8} v {6} h -{1} v -{6} h -{8} z",
                           0, dx, duonaLarĝeco \ 2, alteco, duonaLarĝeco + dx, alteco + Spaceto, alteco - dy, dy,
                           duonaLarĝeco \ 2 - dx \ 2 + Spaceto \ 2)
   End Function

   Private Function PartaTransitiva2() As String
      x += duonaLarĝeco + Spaceto * 2
      Return String.Format("h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{4} {5} h {4} v {7} h -{8} v {6} h {8} v {7} h -{4} v -{7} h {8} v -{6} h -{8} z",
                           0, dx, duonaLarĝeco \ 2, alteco, duonaLarĝeco + dx, alteco + Spaceto, alteco - dy * 2, dy,
                           duonaLarĝeco \ 2 - dx \ 2 + Spaceto \ 2)
   End Function

   Private Function PartaNetransitiva() As String
      x += duonaLarĝeco + Spaceto * 2
      Return String.Format("h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{2} {5} v {6} h {8} v {7} h -{4} v -{7} h {2} v -{6} z",
                           0, dx, duonaLarĝeco \ 2, alteco, duonaLarĝeco + dx, alteco + Spaceto, alteco - dy, dy,
                           duonaLarĝeco \ 2 + Spaceto \ 2 - dx \ 2)
   End Function

   Private Function ModifantoMaldekstra() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{4} v -{3} h -{2} z", duonaLarĝeco, dufojaAlteco, duonaLarĝeco - dx, dufojaAlteco - dy, dx)
   End Function

   Private Function ModifantoDekstra() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} z", duonaLarĝeco, dy, duonaLarĝeco - dx, dufojaAlteco - dy, dx)
   End Function

   Private Function NombrigeblaEcoDekstra() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{2} v {3} h {2} v {1} h -{0} z",
                           duonaLarĝeco, dy, duonaLarĝeco - dx, alteco + Spaceto \ 2 - dy * 3 \ 2)
   End Function

   Private Function NombrigeblaEcoMaldekstra() As String
      x += duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{0} v -{2} h {3} v -{4} h -{3} v {2} h {3} v -{4} h -{3} z",
                           duonaLarĝeco, dufojaAlteco, dy, duonaLarĝeco - dx, alteco + Spaceto \ 2 - dy \ 2)
   End Function

   Private Function MankaNominativo() As String
      x += dx + Spaceto + duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{0} z m {2} 0 h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
                           dx, dufojaAlteco \ 2, dx + Spaceto, duonaLarĝeco - dx)
   End Function

   Private Function PartaNominativo() As String
      x += dx + Spaceto + duonaLarĝeco + Spaceto
      Return String.Format("m 0 {4} h {0} v {1} h -{0} z m {2} -{4} h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
                           dx, dufojaAlteco \ 2, dx + Spaceto, duonaLarĝeco - dx, alteco + Spaceto \ 2)
   End Function

   Private Function PartaAkuzativo() As String
      x += (dx + Spaceto) * 2 + duonaLarĝeco + Spaceto
      Return String.Format("m 0 {4} h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m {2} -{4}
h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
                           dx, dufojaAlteco \ 2, dx + Spaceto, duonaLarĝeco - dx, alteco + Spaceto \ 2)
   End Function

   Private Function PartaDativo() As String
      x += (dx + Spaceto) * 3 + duonaLarĝeco + Spaceto
      Return String.Format("m 0 {4} h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m {2} -{4}
h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
                           dx, dufojaAlteco \ 2, dx + Spaceto, duonaLarĝeco - dx, alteco + Spaceto \ 2)
   End Function

   Public Sub DesegniFinaĵon(finaĵo As String)
      Dim antaŭX = x
      Dim antaŭY = y
      AldoniVojon(finaĵoDesegniloj(finaĵo)(), antaŭX, antaŭY)
   End Sub

   Public Function Vico() As String
      dokumentoLarĝeco = Math.Max(dokumentoLarĝeco, x + spaco)
      x = spaco
      y += dufojaAlteco + spaco
      Return ""
   End Function

   Private Function M(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h {2} v {3} h -{4} v -{5}",
                           dx, aktualaAlteco - dy, aktualaLarĝeco - dx, dy, aktualaLarĝeco, aktualaAlteco)
   End Function

   Private Function P(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} v -{5}",
                           aktualaLarĝeco, dy, aktualaLarĝeco - dx, aktualaAlteco - dy, dx, aktualaAlteco)
   End Function

   Private Function Pl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{6} v -{2} h -{5} v {2} h -{3} z m {9} {7} v {8} h {1} v -{8} z",
                           aktualaLarĝeco, aktualaLarĝeco \ 5, aktualaAlteco - dy, dx, aktualaAlteco,
                           aktualaLarĝeco - (aktualaLarĝeco \ 5 + dx * 3),
                           (aktualaLarĝeco \ 5 + dx * 2), dy, aktualaAlteco - 2 * dy, aktualaAlteco - aktualaAlteco \ 5 - dx * 2)
   End Function

   Private Function Pr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim rSpaco = aktualaLarĝeco \ 5
      Return String.Format("v {4} h {3} v -{2} h {1} v -{5} z m {0} 0 m 0 {5} l -{6} {2} h -{3} l {6} -{2} z m -{7} 0 l {6} {2} h -{3} l -{6} -{2} z",
                           aktualaLarĝeco, aktualaLarĝeco - dx, aktualaAlteco - dy, dx, aktualaAlteco,
                           dy, rSpaco \ 2 + dx \ 2, rSpaco + dx)
   End Function

   Private Function B(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{5} v {2} h -{3} v -{2} h -{1} v {2} h -{3} z",
                           aktualaLarĝeco, dx * 2, aktualaAlteco - dy, dx, dy, aktualaLarĝeco - dx * 4)
   End Function

   Private Function Bl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{6} v -{2} h -{5} v {2} h -{3} v -{2} h -{1} v {2} h -{3} z m {9} {7} v {8} h {1} v -{8} z",
                           aktualaLarĝeco, aktualaLarĝeco \ 5, aktualaAlteco - dy, dx, aktualaAlteco,
                           aktualaLarĝeco - 2 * (aktualaLarĝeco \ 5 + dx * 2),
                           (aktualaLarĝeco \ 5 + dx * 2), dy, aktualaAlteco - 2 * dy, aktualaAlteco - aktualaAlteco \ 5 - dx * 2)
   End Function

   Private Function Br(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim rSpaco = aktualaLarĝeco \ 5
      Return String.Format("v {4} h {3} v -{2} h {8} v {2} h {3} v -{2} h {1} v -{5} z m {0} 0 m 0 {5}
l -{6} {2} h -{3} l {6} -{2} z m -{7} 0 l {6} {2} h -{3} l -{6} -{2} z",
                           aktualaLarĝeco, aktualaLarĝeco - (aktualaLarĝeco \ 5 + 2 * dx),
                           aktualaAlteco - dy, dx, aktualaAlteco,
                           dy, rSpaco \ 2 + dx \ 2, rSpaco + dx, aktualaLarĝeco \ 5)
   End Function

   Private Function V(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m 0 {1} l {2} {3} h {4} l -{2} -{3} z",
                           aktualaLarĝeco, dy, If(aktualaLarĝeco < larĝeco, aktualaLarĝeco - dx, aktualaLarĝeco \ 2 - dx \ 2),
                           aktualaAlteco - dy, dx)
   End Function

   Private Function K(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{3} v -{2} h -{1} z",
                           aktualaLarĝeco, aktualaLarĝeco - dx, aktualaAlteco - dy, dx, aktualaAlteco)
   End Function

   Private Function Kl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim l = aktualaLarĝeco \ 5
      Dim lLarĝeco = l + 2 * dx
      Return String.Format("h {0} v {1} h -{2} v -{3} h -{4} v {3} h -{5} z m {2} {6} v {7} h {8} v -{7} z",
                           aktualaLarĝeco, aktualaAlteco, dx, aktualaAlteco - dy, aktualaLarĝeco - lLarĝeco - dx, lLarĝeco, dy,
                           aktualaAlteco - dy * 2, l)
   End Function

   Private Function Kr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim rSpaco = aktualaLarĝeco \ 5
      Return String.Format("h {0} v {4} h -{3} v -{2} h -{1} z m 0 {5} l {6} {2} h {3} l -{6} -{2} z m {7} 0 l -{6} {2} h {3} l {6} -{2} z",
                           aktualaLarĝeco, aktualaLarĝeco - dx, aktualaAlteco - dy, dx, aktualaAlteco,
                           dy, rSpaco \ 2 + dx \ 2, rSpaco + dx)
   End Function

   Private Function G(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{3} v -{2} h -{1} v {2} h -{3} v -{2} h -{5} z",
                           aktualaLarĝeco, dx * 2, aktualaAlteco - dy, dx, aktualaAlteco, aktualaLarĝeco - dx * 4)
   End Function

   Private Function Gr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim rSpaco = aktualaLarĝeco \ 5
      Return String.Format("h {0} v {4} h -{3} v -{2} h -{1} v {2} h -{3} v -{2} h -{8} z m 0 {5} l {6} {2} h {3} l -{6} -{2} z m {7} 0
l -{6} {2} h {3} l {6} -{2} z",
                           aktualaLarĝeco, rSpaco, aktualaAlteco - dy, dx, aktualaAlteco,
                           dy, rSpaco \ 2 + dx \ 2, rSpaco + dx, aktualaLarĝeco - rSpaco - 2 * dx)
   End Function

   Private Function Gl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{3} v -{2} h -{1} v {2} h -{3} v -{2} h -{5} v {2} h -{6} z m {3} {7} v {8} h {1} v -{8} z",
                           aktualaLarĝeco, aktualaLarĝeco \ 5, aktualaAlteco - dy, dx, aktualaAlteco,
                           aktualaLarĝeco - 2 * (aktualaLarĝeco \ 5 + dx * 2),
                           (aktualaLarĝeco \ 5 + dx * 2), dy, aktualaAlteco - 2 * dy)
   End Function

   Private Function E(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h {2} v {3} h -{2} v {1} h -{0} z",
                           dx, aktualaAlteco \ 2 - dy \ 2, aktualaLarĝeco - dx, dy)
   End Function

   Private Function I(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} z",
                           aktualaLarĝeco, dy, aktualaLarĝeco - dx, aktualaAlteco - dy, dx)
   End Function

   Private Function O(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("m 0 {1} h {2} v -{1} h {0} v {3} h -{0} v -{1} h -{2} z",
                           dx, aktualaAlteco \ 2 - dy \ 2, aktualaLarĝeco - dx, aktualaAlteco)
   End Function

   Private Function AA(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("m 0 {3} h {2} v -{3} h {4} v {5} h -{0} z",
                           aktualaLarĝeco, dy, aktualaLarĝeco - dx, aktualaAlteco - dy, dx, aktualaAlteco)
   End Function

   Private Function N(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {0} h {1} v {3} h -{4}",
                           aktualaAlteco - dy,
                           aktualaLarĝeco \ 2 - dx \ 2,
                           dx,
                           dy,
                           aktualaLarĝeco)
   End Function

   Private Function H(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m 0 {2} h {0} v {1} h -{0} z",
                           aktualaLarĝeco,
                           dy,
                           aktualaAlteco - dy)
   End Function

   Private Function T(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} v -{3} h -{2} z",
                           aktualaLarĝeco,
                           dy,
                           aktualaLarĝeco \ 2 - dx \ 2,
                           aktualaAlteco - dy,
                           dx)
   End Function

   Private Function Tl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} v -{3} h -{2} z m {5} {1} v {6} h {7} v -{6} z",
                           aktualaLarĝeco,
                           dy,
                           aktualaLarĝeco \ 2 - (aktualaLarĝeco \ 5 + 2 * dx) \ 2,
                           aktualaAlteco - dy,
                           aktualaLarĝeco \ 5 + 2 * dx,
                           aktualaLarĝeco \ 2 - (aktualaLarĝeco \ 5 + 2 * dx) \ 2 + dx, aktualaAlteco - 2 * dy,
                           aktualaLarĝeco \ 5)
   End Function

   Private Function Tr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m {2} 0 h {3} l {5} {4} h -{3} z m {6} 0 h {3} l -{5} {4} h -{3} z",
                           aktualaLarĝeco,
                           dy,
                           aktualaLarĝeco \ 2 - (aktualaLarĝeco \ 5 + 2 * dx) \ 2,
                           dx,
                           aktualaAlteco,
                           aktualaLarĝeco \ 10 + dx \ 2,
                           aktualaLarĝeco \ 5 + dx)
   End Function

   Private Function D(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {5} h -{3} v -{5} h -{4} v {5} h -{3} v -{5} h -{2} z",
                           aktualaLarĝeco,
                           dy, aktualaLarĝeco \ 2 - 2 * dx, dx, 2 * dx, aktualaAlteco - dy)
   End Function

   Private Function Dl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim l = aktualaLarĝeco \ 5
      Dim lLarĝeco = aktualaLarĝeco \ 5 + 2 * dx
      Return String.Format("h {0} v {1} h -{2} v -{3} h -{4} v {3} h -{2} z m {5} {6} v {7} h {8} v -{7} z m {9} 0 v {7} h {8} v -{7} z",
                           aktualaLarĝeco, aktualaAlteco, lLarĝeco, aktualaAlteco - dy, aktualaLarĝeco - 2 * lLarĝeco, dx, dy,
                           aktualaAlteco - 2 * dy, l, l + dx * 2 + aktualaLarĝeco - 2 * lLarĝeco)
   End Function

   Private Function Dr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m 0 {1} l {2} {3} h {4} l -{2} -{3} z m {2} 0 v {3} h {4} v -{3} z m {5} 0
h {4} v {3} h -{4} z m 0 {3} l {2} -{3} h {4} l -{2} {3} z",
                           aktualaLarĝeco, dy, aktualaLarĝeco \ 2 - dx * 2, aktualaAlteco - dy, dx, dx * 3)
   End Function

   Private Function S(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m 0 {2} l {3} -{2} h {4} l -{3} {2} z m {3} -{2} h {4} l {3} {2} h -{4}",
                           aktualaLarĝeco, dy, aktualaAlteco, aktualaLarĝeco \ 2 - dx \ 2, dx)
   End Function

   Private Function R(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m 0 {1} l {2} {3} h {4} l -{2} -{3} z m {0} 0 h -{4} l -{2} {3} h {4} z",
                           aktualaLarĝeco, dy, aktualaLarĝeco \ 2 - dx \ 2,
                           aktualaAlteco - dy, dx)
   End Function

   Private Function L(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m {2} {3} v {4} h {5} v -{4} z",
                           aktualaLarĝeco, aktualaAlteco, dx, dy, aktualaAlteco - 2 * dy, aktualaLarĝeco - 2 * dx)
   End Function

   Private Function J(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{0} v -{1} h {2} v -{3} h -{2} z",
                           aktualaLarĝeco,
                           dy,
                           aktualaLarĝeco \ 2 - dx \ 2,
                           aktualaAlteco - dy * 2)
   End Function

   Private Function W(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("m {0} 0 a {0} {1} 0 0 0 0 {2} v -{3} a {4} {5} 0 0 1 0 -{6} z a {0} {1} 0 0 1 0 {2} v -{3} a {4} {5} 0 0 0 0 -{6} z",
                           aktualaLarĝeco \ 2, aktualaAlteco \ 2, aktualaAlteco, dy, aktualaLarĝeco \ 2 - dx, aktualaAlteco \ 2 - dy, aktualaAlteco - dy * 2)
   End Function

   Private Function ĈuVokalo(litero As Char) As Boolean
      Return litero = "a"c Or
         litero = "e"c Or
         litero = "i"c Or
         litero = "o"c Or
         litero = "u"c Or
         litero = "ɒ"c
   End Function

   Private Sub AldoniVojon(vojo As String, x As Integer, y As Integer)
      vojoj.Add(String.Format("M {0} {1} {2}", x, y, vojo))
   End Sub

   Public Sub Fini()
      xmlSkribilo.WriteStartDocument()
      xmlSkribilo.WriteStartElement("svg", "http://www.w3.org/2000/svg")
      xmlSkribilo.WriteAttributeString("width", Math.Max(x + spaco, dokumentoLarĝeco))
      xmlSkribilo.WriteAttributeString("height", dufojaAlteco + y + 2 * spaco)

      For Each vojo In vojoj
         xmlSkribilo.WriteStartElement("path")
         xmlSkribilo.WriteAttributeString("d", vojo)
         xmlSkribilo.WriteEndElement()
      Next

      xmlSkribilo.WriteEndElement()
      xmlSkribilo.WriteEndDocument()
      xmlSkribilo.Close()
   End Sub
End Class
