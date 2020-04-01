Imports System.Xml

Public Class SvgDesegnilo
   Private ReadOnly _xmlSkribilo As XmlWriter
   Private ReadOnly _alteco, _larĝeco As Integer
   Private ReadOnly _dufojaAlteco, _duonaLarĝeco As Integer
   Private _x, _y As Double
   Private _dokumentoLarĝeco As Integer

   Private ReadOnly _literoDesegniloj As Dictionary(Of String, LiteroDesegnilo) =
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

   Private ReadOnly _finaĵoDesegniloj As Dictionary(Of String, FinaĵoDesegnilo) =
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
      {"netransitiva", AddressOf Netransitiva},
      {"nedirektaTransitiva", AddressOf NedirecktaTransitiva},
      {"transitiva", AddressOf Transitiva},
      {"dutransitiva", AddressOf Dutransitiva},
      {"imperativo", AddressOf Imperativo},
      {"invito", AddressOf Invito},
      {"difinito", AddressOf Difinito},
      {"unuNombro", AddressOf UnuNombro},
      {"pluraNombro", AddressOf PluraNombro},
      {"havaĵo", AddressOf Havaĵo},
      {"malplenaVerbo", AddressOf MalplenaVerbo},
      {"oblikaNetransitiva", AddressOf OblikaNetransitiva},
      {"oblikaTransitiva", AddressOf OblikaTransitiva},
      {"nedirektaNetransitiva", AddressOf NedirektaNetransitiva},
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
      {"argumento1", AddressOf Argumento1},
      {"argumento2", AddressOf Argumento2},
      {"argumento3", AddressOf Argumento3},
      {"translativo", AddressOf Translativo},
      {"ujo1Unue", AddressOf Ujo1Unue},
      {"ujo2Unue", AddressOf Ujo2Unue},
      {"ujo3Unue", AddressOf Ujo3Unue},
      {"igo", AddressOf Igo},
      {"etigo", AddressOf Etigo}
      }

   Private ReadOnly _vojoj As List(Of String) = New List(Of String)
   Private ReadOnly _dx As Integer
   Private ReadOnly _dy As Integer
   Private ReadOnly _spaco As Integer

   Private ReadOnly Property Spaceto As Integer
      Get
         Return _spaco/2
      End Get
   End Property

   Private Delegate Function LiteroDesegnilo(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String

   Private Delegate Function FinaĵoDesegnilo() As String

   Public Sub New(elirejo As String, alteco As Integer, larĝeco As Integer, dx As Integer, dy As Integer,
                  Optional spaco As Integer = 8)
      _xmlSkribilo = XmlWriter.Create(elirejo)
      _alteco = alteco\2
      _larĝeco = larĝeco + Spaceto
      _dx = dx
      _dy = dy
      _spaco = spaco
      _dufojaAlteco = alteco + Spaceto
      _duonaLarĝeco = larĝeco\2
      _dokumentoLarĝeco = 0
      _x = 10
      _y = 10
   End Sub

   Public Sub DesegniSilabon(silabo As String)
      If silabo.Length = 1 Then
         AldoniVojon(_literoDesegniloj(silabo)(_larĝeco, _dufojaAlteco), _x, _y)
         _x += _larĝeco + Spaceto
      ElseIf silabo.Length = 2 Then
         If ĈuVokalo(silabo.Chars(0)) Then 'VC
            AldoniVojon(_literoDesegniloj(silabo.Substring(0, 1))(_duonaLarĝeco, _dufojaAlteco), _x, _y)
            _x += _larĝeco\2 + Spaceto
            AldoniVojon(_literoDesegniloj(silabo.Substring(1, 1))(_duonaLarĝeco, _dufojaAlteco), _x, _y)
            _x += _larĝeco\2 + Spaceto
         Else 'CV
            AldoniVojon(_literoDesegniloj(silabo.Substring(0, 1))(_larĝeco, _alteco), _x, _y)
            Dim antaŭY = _y
            _y += _alteco + Spaceto
            AldoniVojon(_literoDesegniloj(silabo.Substring(1, 1))(_larĝeco, _alteco), _x, _y)
            _x += _larĝeco + Spaceto
            _y = antaŭY
         End If
      ElseIf silabo.Length = 3 Then
         If silabo.Chars(1) = "r"c Or silabo.Chars(1) = "l"c Then 'CCV
            AldoniVojon(_literoDesegniloj(silabo.Substring(0, 2))(_larĝeco, _alteco), _x, _y)
            Dim antaŭY = _y
            _y += _alteco + Spaceto
            AldoniVojon(_literoDesegniloj(silabo.Substring(2, 1))(_larĝeco, _alteco), _x, _y)
            _x += _larĝeco + Spaceto
            _y = antaŭY
         Else 'CVC
            AldoniVojon(_literoDesegniloj(silabo.Substring(0, 1))(_larĝeco, _alteco), _x, _y)
            Dim antaŭY = _y
            _y += _alteco + Spaceto
            AldoniVojon(_literoDesegniloj(silabo.Substring(1, 1))(_duonaLarĝeco, _alteco), _x, _y)
            _x += _larĝeco\2 + Spaceto
            AldoniVojon(_literoDesegniloj(silabo.Substring(2, 1))(_duonaLarĝeco, _alteco), _x, _y)
            _x += _larĝeco\2 + Spaceto
            _y = antaŭY
         End If
      ElseIf silabo.Length = 4 Then 'CCVC
         AldoniVojon(_literoDesegniloj(silabo.Substring(0, 2))(_larĝeco, _alteco), _x, _y)
         Dim antaŭY = _y
         _y += _alteco + Spaceto
         AldoniVojon(_literoDesegniloj(silabo.Substring(2, 1))(_duonaLarĝeco, _alteco), _x, _y)
         _x += _larĝeco\2 + Spaceto
         AldoniVojon(_literoDesegniloj(silabo.Substring(3, 1))(_duonaLarĝeco, _alteco), _x, _y)
         _x += _larĝeco\2 + Spaceto
         _y = antaŭY
      Else
         Throw New Exception($"Nevalida silabo: {silabo}")
      End If
   End Sub

   Public Function NomoKomenco() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{0} z",
                           _duonaLarĝeco, _dy, _duonaLarĝeco - _dx, _dufojaAlteco - _dy*2)
   End Function

   Public Function NomoFino() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{0} v -{2} h {3} v -{4} h -{3} z",
                           _duonaLarĝeco, _dufojaAlteco, _dy, _duonaLarĝeco - _dx, _dufojaAlteco - _dy*2)
   End Function

   Public Function Lokokupilo() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {0} h {3} v -{0} h {2} v {4} h -{5} z",
                           _dufojaAlteco - _dy, _duonaLarĝeco\2 - _dx\2, _dx, _duonaLarĝeco\2 - _dx*3\2, _dufojaAlteco,
                           _duonaLarĝeco)
   End Function

   Private Function EcoDekstra() As String
      _x += _duonaLarĝeco + Spaceto
      Return _
         String.Format("m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m 0 {7} h -{0} v {8} h {0} z",
                       _duonaLarĝeco, _alteco, _dufojaAlteco, _dufojaAlteco - _dy, _duonaLarĝeco - _dx, _alteco - _dy,
                       _dufojaAlteco - 2*_dy,
                       _alteco - _dy\2, _dy)
   End Function

   Private Function EcoMaldekstra() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("a {0} {1} 0 0 1 0 {2} z v {3} a {4} {5} 0 0 0 0 -{6} z m 0 {7} h {0} v {8} h -{0} z",
                           _duonaLarĝeco, _alteco, _dufojaAlteco, _dufojaAlteco - _dy, _duonaLarĝeco - _dx,
                           _alteco - _dy,
                           _dufojaAlteco - 2*_dy,
                           _alteco - _dy\2, _dy)
   End Function

   Private Function RekordoMaldekstra() As String
      _x += _duonaLarĝeco + Spaceto*2 + _dx
      Return _
         String.Format(
            "a {0} {1} 0 0 1 0 {2} z v {3} a {4} {5} 0 0 0 0 -{6} z m {7} 0 h {8} v {1} h -{8} z m 0 {9} h {8} v {1} h -{8} z",
            _duonaLarĝeco, _alteco, _dufojaAlteco, _dufojaAlteco - _dy, _duonaLarĝeco - _dx, _alteco - _dy,
            _dufojaAlteco - 2*_dy, _duonaLarĝeco + Spaceto, _dx, _alteco + Spaceto)
   End Function

   Private Function RekordoDekstra() As String
      _x += _duonaLarĝeco + Spaceto*2 + _dx
      Return _
         String.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} 0 h {8} v {1} h -{8} z m 0 {9} h {8} v {1} h -{8} z",
            _duonaLarĝeco, _alteco, _dufojaAlteco, _dufojaAlteco - _dy, _duonaLarĝeco - _dx, _alteco - _dy,
            _dufojaAlteco - 2*_dy, Spaceto, _dx, _alteco + Spaceto)
   End Function

   Private Function PredikativoEsti() As String
      _x += _duonaLarĝeco + Spaceto
      Return _
         String.Format("m 0 {3} h {5} v {6} h -{5} z m {1} -{3} h {2} v {4} h -{2} z m {7} 0  h {2} v {4} h -{2} z",
                       _dufojaAlteco - _dy, _duonaLarĝeco\2 - _dx\2, _dx, _dufojaAlteco\2 - _dy\2, _dufojaAlteco,
                       _duonaLarĝeco, _dy, _duonaLarĝeco\2)
   End Function

   Private Function Pridiranto() As String
      _x += _duonaLarĝeco + Spaceto
      Return _
         String.Format(
            "m 0 {3} h {0} v {1} h -{0} z l {2} -{3} h {4} l -{2} {3} z m {2} -{3} h {4} l {2} {3} h -{4} z",
            _duonaLarĝeco, _dy, _duonaLarĝeco\2 - _dx\2, _dufojaAlteco - _dy, _dx)
   End Function

   Private Function Etigo() As String
      _x += _duonaLarĝeco + Spaceto
      Return _
         String.Format(
            "m 0 {5} h {0} v {1} h -{0} z l {2} -{3} h {4} l -{2} {3} z m {2} -{3} h {4} l {2} {3} h -{4} z",
            _duonaLarĝeco, _dy, _duonaLarĝeco\2 - _dx\2, _dufojaAlteco\2 - _dy, _dx, _dufojaAlteco - _dy)
   End Function

   Private Function Netransitiva() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {0} h {1} v {3} h -{1} v {0} h -{2} v -{0} h -{1} z",
                           _dufojaAlteco\2 - _dy\2, _duonaLarĝeco\2 - _dx\2, _dx, _dy)
   End Function

   Private Function NedirecktaTransitiva() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{0} v -{1} h {2} v -{3} h -{2} z",
                           _duonaLarĝeco, _dy, _duonaLarĝeco\2 - _dx\2, _dufojaAlteco - 2*_dy)
   End Function

   Private Function Transitiva() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} l -{2} {3} h {2} v {1} h -{0} v -{1} l {2} -{3} h -{2} z",
                           _duonaLarĝeco, _dy, _duonaLarĝeco - _dx, _dufojaAlteco - 2*_dy)
   End Function

   Private Function Dutransitiva() As String
      _x += _duonaLarĝeco + Spaceto
      Return _
         String.Format(
            "h {0} v {1} l -{2} {3} h {2} v {1} h -{0} v -{1} l {2} -{3} h -{2} z m 0 {4} h {0} v {1} h -{0} z",
            _duonaLarĝeco, _dy, _duonaLarĝeco - _dx, _dufojaAlteco - 2*_dy, _dufojaAlteco\2 - _dy\2)
   End Function

   Private Function Perfekto() As String
      _x += _dx + _duonaLarĝeco\2 + Spaceto*2
      Return String.Format("h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z m {5} 0 h {0} v {1} h -{0} z",
                           _dx, _dufojaAlteco, _dx*2, _duonaLarĝeco\2, _alteco + Spaceto\2, _duonaLarĝeco\2 + _dx)
   End Function

   Private Function Progresivo() As String
      _x += _dx*2 + _duonaLarĝeco\2 + Spaceto
      Return _
         String.Format(
            "h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
            _dx, _dufojaAlteco, _dx*2, _duonaLarĝeco\2, _alteco + Spaceto\2)
   End Function

   Private Function Argumento1() As String
      _x += _dx*2 + _duonaLarĝeco\2 + Spaceto
      Return _
         String.Format(
            "m {3} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z m {2} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z",
            _dx, _dufojaAlteco, _dx*2, _duonaLarĝeco\2, _alteco + Spaceto\2)
   End Function

   Private Function Argumento2() As String
      _x += _dx + _duonaLarĝeco + Spaceto
      Return _
         String.Format(
            "m {3} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
            _dx, _dufojaAlteco, _dx*2, _duonaLarĝeco\2, _alteco + Spaceto\2)
   End Function

   Private Function Argumento3() As String
      _x += _dx*2 + _duonaLarĝeco + Spaceto*2
      Return _
         String.Format(
            "m {3} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z
m {1} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
            _dx, _dx + Spaceto, _dx*2, _duonaLarĝeco\2, _alteco + Spaceto\2)
   End Function

   Private Function Estonteco() As String
      _x += Spaceto*2 + _duonaLarĝeco\2 + _dx*2
      Return String.Format("h {0} v {1} h -{0} z m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
                           _dx, _dufojaAlteco, _dx*2, _duonaLarĝeco\2, _alteco + Spaceto\2)
   End Function

   Private Function Igo() As String
      _x += _dx*3 + _duonaLarĝeco + Spaceto
      Return _
         String.Format(
            "h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z m {5} 0 h {0} v {1} h -{0} z
m {5} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z",
            _dx, _dufojaAlteco, _dx*2, _duonaLarĝeco\2, _alteco + Spaceto\2, _duonaLarĝeco\2 + _dx)
   End Function

   Private Function Translativo() As String
      _x += Spaceto + _duonaLarĝeco\2 + _dx*2
      Return String.Format("h {0} v {1} h -{0} z m {5} 0 h {0} l -{3} {4} l {3} {4} h -{0} l -{3} -{4} z",
                           _dx, _dufojaAlteco, _dx*2, _duonaLarĝeco\2, _alteco + Spaceto\2, _dx + _duonaLarĝeco\2)
   End Function

   Private Function Ekzistado() As String
      _x += _duonaLarĝeco + Spaceto
      Return _
         String.Format("h {0} v {1} l -{2} {3} l {2} {3} v {1} h -{0} v -{1} h {2} l -{2} -{3} l {2} -{3} h -{2} z",
                       _duonaLarĝeco, _dy, _duonaLarĝeco - _dx, _dufojaAlteco\2 - _dy)
   End Function

   Private Function Imperativo() As String
      _x += _dx*3 + _duonaLarĝeco + Spaceto
      Return _
         String.Format(
            "h {0} v {1} h -{0} z m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z
m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
            _dx, _dufojaAlteco, _dx*2, _duonaLarĝeco\2, _alteco + Spaceto\2)
   End Function

   Private Function Invito() As String
      _x += _dx*3 + _duonaLarĝeco + Spaceto
      Return _
         String.Format(
            "h {0} v {1} h -{0} z m 0 {5} h {0} v {1} h -{0} z m {2} -{5} h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z
m {2} 0 h {0} l {3} {4} l -{3} {4} h -{0} l {3} -{4} z",
            _dx, _alteco, _dx*2, _duonaLarĝeco\2, _alteco + Spaceto\2, _alteco + Spaceto)
   End Function

   Private Function Volo1() As String
      _x += _duonaLarĝeco + Spaceto*2 + _dx
      Return _
         String.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} {9} h {8} v {1} h -{8} z",
            _duonaLarĝeco, _alteco, _dufojaAlteco, _dufojaAlteco - _dy, _duonaLarĝeco - _dx, _alteco - _dy,
            _dufojaAlteco - 2*_dy, Spaceto, _dx, _alteco + Spaceto)
   End Function

   Private Function Volo2() As String
      _x += _duonaLarĝeco + Spaceto*3 + _dx*2
      Return _
         String.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} {9} h {8} v {1} h -{8} z m {10} 0 h {8} v {1} h -{8} z",
            _duonaLarĝeco, _alteco, _dufojaAlteco, _dufojaAlteco - _dy, _duonaLarĝeco - _dx, _alteco - _dy,
            _dufojaAlteco - 2*_dy, Spaceto, _dx, _alteco + Spaceto, _dx + Spaceto)
   End Function

   Private Function Volo3() As String
      _x += _duonaLarĝeco + Spaceto*4 + _dx*3
      Return _
         String.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m {7} {9} h {8} v {1} h -{8} z
m {10} 0 h {8} v {1} h -{8} z m {10} 0 h {8} v {1} h -{8} z",
            _duonaLarĝeco, _alteco, _dufojaAlteco, _dufojaAlteco - _dy, _duonaLarĝeco - _dx, _alteco - _dy,
            _dufojaAlteco - 2*_dy, Spaceto, _dx, _alteco + Spaceto, _dx + Spaceto)
   End Function

   Private Function Sola() As String
      _x += _duonaLarĝeco + Spaceto
      Return _
         String.Format(
            "h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m -{2} {3} h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z",
            _dx, _alteco, _duonaLarĝeco - _dx, _alteco + Spaceto)
   End Function

   Private Function Klaso() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("m {0} 0 h {1} l -{0} {2} l {0} {2} h -{1} l -{0} -{2} z",
                           _duonaLarĝeco - _dx, _dx, _dufojaAlteco\2)
   End Function

   Private Function Difinito() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} l {1} -{0} h {3} l -{1} {0} h {1} v {2} h -{1} l {1} {0} h -{3} l -{1} -{0} z",
                           _dufojaAlteco\2 - _dy\2, _duonaLarĝeco - _dx, _dy, _dx)
   End Function

   Private Function UnuNombro() As String
      _x += _duonaLarĝeco + Spaceto*2 + _dx
      Return _
         String.Format(
            "m 0 {0} l {1} -{0} h {3} l -{1} {0} h {1} v {2} h -{1} l {1} {0} h -{3} l -{1} -{0} z m {4} -{0} h {3} v {5} h -{3} z",
            _dufojaAlteco\2 - _dy\2, _duonaLarĝeco - _dx, _dy, _dx, _duonaLarĝeco + Spaceto, _alteco)
   End Function

   Private Function PluraNombro() As String
      _x += _duonaLarĝeco + Spaceto*2 + _dx
      Return _
         String.Format(
            "m 0 {0} l {1} -{0} h {3} l -{1} {0} h {1} v {2} h -{1} l {1} {0} h -{3} l -{1} -{0} z m {4} -{0} h {3} v {5} h -{3} z
m 0 {6} h {3} v {5} h -{3} z",
            _dufojaAlteco\2 - _dy\2, _duonaLarĝeco - _dx, _dy, _dx, _duonaLarĝeco + Spaceto, _alteco, Spaceto + _alteco)
   End Function

   Private Function Havaĵo() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {3} h -{2} v -{0} h -{1} z",
                           _dufojaAlteco\2 - _dy\2, _duonaLarĝeco - _dx, _dx, _dufojaAlteco)
   End Function

   Private Function AtributivoEstiMaldekstra() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {3} h -{2} v -{0} h -{1} v -{4} h {1} v -{4} h -{1} z",
                           (_dufojaAlteco - 3*_dy)\2, _duonaLarĝeco - _dx, _dx, _dufojaAlteco, _dy)
   End Function

   Private Function AtributivoEstiDekstra() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("h {2} v {0} h {1} v {4} h -{1} v {4} h {1} v {4} h -{1} v {0} h -{2} z",
                           (_dufojaAlteco - 3*_dy)\2, _duonaLarĝeco - _dx, _dx, _dufojaAlteco, _dy)
   End Function

   Private Function Ĝerundo() As String
      _x += _duonaLarĝeco + _dx*2 + Spaceto
      Return _
         String.Format(
            "h {3} l {1} {0} h {3} l -{1} -{0} h {3} l {1} {0} v {2} l -{1} {0} h -{3} l {1} -{0} h -{3} l -{1} {0}
h -{3} l {1} -{0} h -{1} v -{2} h {1} z",
            _dufojaAlteco\2 - _dy\2, _duonaLarĝeco - _dx, _dy, _dx)
   End Function

   Private Function Havado() As String
      _x += _duonaLarĝeco + _dx*2 + Spaceto
      Return _
         String.Format(
            "m {1} 0 h {3} l -{1} {0} h {3} l {1} -{0} h {3} l -{1} {0} v {2} l {1} {0} h -{3}
l -{1} -{0} h -{3} l {1} {0} h -{3} l -{1} -{0} v -{2} z",
            _dufojaAlteco\2 - _dy\2, _duonaLarĝeco - _dx, _dy, _dx)
   End Function

   Private Function MalplenaVerbo() As String
      _x += _duonaLarĝeco + Spaceto*2
      Return _
         String.Format(
            "h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{5} {6} h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z",
            0, _dx, _duonaLarĝeco\2, _alteco, _duonaLarĝeco + _dx, _duonaLarĝeco + _dx, _alteco + Spaceto)
   End Function

   Private Function OblikaNetransitiva() As String
      _x += _duonaLarĝeco + Spaceto*2
      Return _
         String.Format(
            "h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{4} {5} h {4} v {7} h -{8} v {6} h -{1} v -{6} h -{8} z",
            0, _dx, _duonaLarĝeco\2, _alteco, _duonaLarĝeco + _dx, _alteco + Spaceto, _alteco - _dy, _dy,
            _duonaLarĝeco\2 - _dx\2 + Spaceto\2)
   End Function

   Private Function OblikaTransitiva() As String
      _x += _duonaLarĝeco + Spaceto*2
      Return _
         String.Format(
            "h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{4} {5} h {4} v {7} h -{8} v {6} h {8} v {7} h -{4} v -{7} h {8} v -{6} h -{8} z",
            0, _dx, _duonaLarĝeco\2, _alteco, _duonaLarĝeco + _dx, _alteco + Spaceto, _alteco - _dy*2, _dy,
            _duonaLarĝeco\2 - _dx\2 + Spaceto\2)
   End Function

   Private Function NedirektaNetransitiva() As String
      _x += _duonaLarĝeco + Spaceto*2
      Return _
         String.Format(
            "h {1} l {2} {3} h -{1} z m {4} 0 l -{2} {3} h -{1} l {2} -{3} z
m -{2} {5} v {6} h {8} v {7} h -{4} v -{7} h {2} v -{6} z",
            0, _dx, _duonaLarĝeco\2, _alteco, _duonaLarĝeco + _dx, _alteco + Spaceto, _alteco - _dy, _dy,
            _duonaLarĝeco\2 + Spaceto\2 - _dx\2)
   End Function

   Private Function ModifantoMaldekstra() As String
      _x += _duonaLarĝeco + Spaceto
      Return _
         String.Format("h {0} v {1} h -{4} v -{3} h -{2} z", _duonaLarĝeco, _dufojaAlteco, _duonaLarĝeco - _dx,
                       _dufojaAlteco - _dy, _dx)
   End Function

   Private Function ModifantoDekstra() As String
      _x += _duonaLarĝeco + Spaceto
      Return _
         String.Format("h {0} v {1} h -{2} v {3} h -{4} z", _duonaLarĝeco, _dy, _duonaLarĝeco - _dx, _dufojaAlteco - _dy,
                       _dx)
   End Function

   Private Function NombrigeblaEcoDekstra() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{2} v {3} h {2} v {1} h -{0} z",
                           _duonaLarĝeco, _dy, _duonaLarĝeco - _dx, _alteco + Spaceto\2 - _dy*3\2)
   End Function

   Private Function NombrigeblaEcoMaldekstra() As String
      _x += _duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{0} v -{2} h {3} v -{4} h -{3} v {2} h {3} v -{4} h -{3} z",
                           _duonaLarĝeco, _dufojaAlteco, _dy, _duonaLarĝeco - _dx, _alteco + Spaceto\2 - _dy\2)
   End Function

   Private Function MankaNominativo() As String
      _x += _dx + Spaceto + _duonaLarĝeco + Spaceto
      Return String.Format("h {0} v {1} h -{0} z m {2} 0 h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
                           _dx, _dufojaAlteco\2, _dx + Spaceto, _duonaLarĝeco - _dx)
   End Function

   Private Function Ujo1Unue() As String
      _x += _dx + Spaceto + _duonaLarĝeco + Spaceto
      Return String.Format("m 0 {4} h {0} v {1} h -{0} z m {2} -{4} h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
                           _dx, _dufojaAlteco\2, _dx + Spaceto, _duonaLarĝeco - _dx, _alteco + Spaceto\2)
   End Function

   Private Function Ujo2Unue() As String
      _x += (_dx + Spaceto)*2 + _duonaLarĝeco + Spaceto
      Return _
         String.Format(
            "m 0 {4} h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m {2} -{4}
h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
            _dx, _dufojaAlteco\2, _dx + Spaceto, _duonaLarĝeco - _dx, _alteco + Spaceto\2)
   End Function

   Private Function Ujo3Unue() As String
      _x += (_dx + Spaceto)*3 + _duonaLarĝeco + Spaceto
      Return _
         String.Format(
            "m 0 {4} h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m {2} 0 h {0} v {1} h -{0} z m {2} -{4}
h {0} l {3} {1} l -{3} {1} h -{0} l {3} -{1} z",
            _dx, _dufojaAlteco\2, _dx + Spaceto, _duonaLarĝeco - _dx, _alteco + Spaceto\2)
   End Function

   Public Sub DesegniFinaĵon(finaĵo As String)
      Dim antaŭX = _x
      Dim antaŭY = _y
      AldoniVojon(_finaĵoDesegniloj(finaĵo)(), antaŭX, antaŭY)
   End Sub

   Public Function Vico() As String
      _dokumentoLarĝeco = Math.Max(_dokumentoLarĝeco, _x + _spaco)
      _x = _spaco
      _y += _dufojaAlteco + _spaco
      Return ""
   End Function

   Private Function M(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h {2} v {3} h -{4} v -{5}",
                           _dx, aktualaAlteco - _dy, aktualaLarĝeco - _dx, _dy, aktualaLarĝeco, aktualaAlteco)
   End Function

   Private Function P(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} v -{5}",
                           aktualaLarĝeco, _dy, aktualaLarĝeco - _dx, aktualaAlteco - _dy, _dx, aktualaAlteco)
   End Function

   Private Function Pl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{6} v -{2} h -{5} v {2} h -{3} z m {9} {7} v {8} h {1} v -{8} z",
                           aktualaLarĝeco, aktualaLarĝeco\5, aktualaAlteco - _dy, _dx, aktualaAlteco,
                           aktualaLarĝeco - (aktualaLarĝeco\5 + _dx*3),
                           (aktualaLarĝeco\5 + _dx*2), _dy, aktualaAlteco - 2*_dy,
                           aktualaAlteco - aktualaAlteco\5 - _dx*3)
   End Function

   Private Function Pr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim rSpaco = aktualaLarĝeco\5
      Return _
         String.Format(
            "v {4} h {3} v -{2} h {1} v -{5} z m {0} 0 m 0 {5} l -{6} {2} h -{3} l {6} -{2} z m -{7} 0 l {6} {2} h -{3} l -{6} -{2} z",
            aktualaLarĝeco, aktualaLarĝeco - _dx, aktualaAlteco - _dy, _dx, aktualaAlteco,
            _dy, rSpaco\2 + _dx\2, rSpaco + _dx)
   End Function

   Private Function B(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{5} v {2} h -{3} v -{2} h -{1} v {2} h -{3} z",
                           aktualaLarĝeco, _dx*2, aktualaAlteco - _dy, _dx, _dy, aktualaLarĝeco - _dx*4)
   End Function

   Private Function Bl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return _
         String.Format(
            "h {0} v {4} h -{6} v -{2} h -{5} v {2} h -{3} v -{2} h -{1} v {2} h -{3} z m {9} {7} v {8} h {1} v -{8} z",
            aktualaLarĝeco, aktualaLarĝeco\5, aktualaAlteco - _dy, _dx, aktualaAlteco,
            aktualaLarĝeco - 2*(aktualaLarĝeco\5 + _dx*2),
            (aktualaLarĝeco\5 + _dx*2), _dy, aktualaAlteco - 2*_dy, aktualaAlteco - aktualaAlteco\5 - _dx*3)
   End Function

   Private Function Br(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim rSpaco = aktualaLarĝeco\5
      Return _
         String.Format(
            "v {4} h {3} v -{2} h {8} v {2} h {3} v -{2} h {1} v -{5} z m {0} 0 m 0 {5}
l -{6} {2} h -{3} l {6} -{2} z m -{7} 0 l {6} {2} h -{3} l -{6} -{2} z",
            aktualaLarĝeco, aktualaLarĝeco - (aktualaLarĝeco\5 + 2*_dx),
            aktualaAlteco - _dy, _dx, aktualaAlteco,
            _dy, rSpaco\2 + _dx\2, rSpaco + _dx, aktualaLarĝeco\5)
   End Function

   Private Function V(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m 0 {1} l {2} {3} h {4} l -{2} -{3} z",
                           aktualaLarĝeco, _dy,
                           If(aktualaLarĝeco < _larĝeco, aktualaLarĝeco - _dx, aktualaLarĝeco\2 - _dx\2),
                           aktualaAlteco - _dy, _dx)
   End Function

   Private Function K(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{3} v -{2} h -{1} z",
                           aktualaLarĝeco, aktualaLarĝeco - _dx, aktualaAlteco - _dy, _dx, aktualaAlteco)
   End Function

   Private Function Kl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim l = aktualaLarĝeco\5
      Dim lLarĝeco = l + 2*_dx
      Return String.Format("h {0} v {1} h -{2} v -{3} h -{4} v {3} h -{5} z m {2} {6} v {7} h {8} v -{7} z",
                           aktualaLarĝeco, aktualaAlteco, _dx, aktualaAlteco - _dy, aktualaLarĝeco - lLarĝeco - _dx,
                           lLarĝeco, _dy,
                           aktualaAlteco - _dy*2, l)
   End Function

   Private Function Kr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim rSpaco = aktualaLarĝeco\5
      Return _
         String.Format(
            "h {0} v {4} h -{3} v -{2} h -{1} z m 0 {5} l {6} {2} h {3} l -{6} -{2} z m {7} 0 l -{6} {2} h {3} l {6} -{2} z",
            aktualaLarĝeco, aktualaLarĝeco - _dx, aktualaAlteco - _dy, _dx, aktualaAlteco,
            _dy, rSpaco\2 + _dx\2, rSpaco + _dx)
   End Function

   Private Function G(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {4} h -{3} v -{2} h -{1} v {2} h -{3} v -{2} h -{5} z",
                           aktualaLarĝeco, _dx*2, aktualaAlteco - _dy, _dx, aktualaAlteco, aktualaLarĝeco - _dx*4)
   End Function

   Private Function Gr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim rSpaco = aktualaLarĝeco\5
      Return _
         String.Format(
            "h {0} v {4} h -{3} v -{2} h -{1} v {2} h -{3} v -{2} h -{8} z m 0 {5} l {6} {2} h {3} l -{6} -{2} z m {7} 0
l -{6} {2} h {3} l {6} -{2} z",
            aktualaLarĝeco, rSpaco, aktualaAlteco - _dy, _dx, aktualaAlteco,
            _dy, rSpaco\2 + _dx\2, rSpaco + _dx, aktualaLarĝeco - rSpaco - 2*_dx)
   End Function

   Private Function Gl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return _
         String.Format(
            "h {0} v {4} h -{3} v -{2} h -{1} v {2} h -{3} v -{2} h -{5} v {2} h -{6} z m {3} {7} v {8} h {1} v -{8} z",
            aktualaLarĝeco, aktualaLarĝeco\5, aktualaAlteco - _dy, _dx, aktualaAlteco,
            aktualaLarĝeco - 2*(aktualaLarĝeco\5 + _dx*2),
            (aktualaLarĝeco\5 + _dx*2), _dy, aktualaAlteco - 2*_dy)
   End Function

   Private Function E(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h {2} v {3} h -{2} v {1} h -{0} z",
                           _dx, aktualaAlteco\2 - _dy\2, aktualaLarĝeco - _dx, _dy)
   End Function

   Private Function I(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} z",
                           aktualaLarĝeco, _dy, aktualaLarĝeco - _dx, aktualaAlteco - _dy, _dx)
   End Function

   Private Function O(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("m 0 {1} h {2} v -{1} h {0} v {3} h -{0} v -{1} h -{2} z",
                           _dx, aktualaAlteco\2 - _dy\2, aktualaLarĝeco - _dx, aktualaAlteco)
   End Function

   Private Function AA(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("m 0 {3} h {2} v -{3} h {4} v {5} h -{0} z",
                           aktualaLarĝeco, _dy, aktualaLarĝeco - _dx, aktualaAlteco - _dy, _dx, aktualaAlteco)
   End Function

   Private Function N(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {0} h {1} v {3} h -{4}",
                           aktualaAlteco - _dy,
                           aktualaLarĝeco\2 - _dx\2,
                           _dx,
                           _dy,
                           aktualaLarĝeco)
   End Function

   Private Function H(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m 0 {2} h {0} v {1} h -{0} z",
                           aktualaLarĝeco,
                           _dy,
                           aktualaAlteco - _dy)
   End Function

   Private Function T(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} v -{3} h -{2} z",
                           aktualaLarĝeco,
                           _dy,
                           aktualaLarĝeco\2 - _dx\2,
                           aktualaAlteco - _dy,
                           _dx)
   End Function

   Private Function Tl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} v -{3} h -{2} z m {5} {1} v {6} h {7} v -{6} z",
                           aktualaLarĝeco,
                           _dy,
                           aktualaLarĝeco\2 - (aktualaLarĝeco\5 + 2*_dx)\2,
                           aktualaAlteco - _dy,
                           aktualaLarĝeco\5 + 2*_dx,
                           aktualaLarĝeco\2 - (aktualaLarĝeco\5 + 2*_dx)\2 + _dx, aktualaAlteco - 2*_dy,
                           aktualaLarĝeco\5)
   End Function

   Private Function Tr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m {2} 0 h {3} l {5} {4} h -{3} z m {6} 0 h {3} l -{5} {4} h -{3} z",
                           aktualaLarĝeco,
                           _dy,
                           aktualaLarĝeco\2 - (aktualaLarĝeco\5 + 2*_dx)\2,
                           _dx,
                           aktualaAlteco,
                           aktualaLarĝeco\10 + _dx\2,
                           aktualaLarĝeco\5 + _dx)
   End Function

   Private Function D(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {5} h -{3} v -{5} h -{4} v {5} h -{3} v -{5} h -{2} z",
                           aktualaLarĝeco,
                           _dy, aktualaLarĝeco\2 - 2*_dx, _dx, 2*_dx, aktualaAlteco - _dy)
   End Function

   Private Function Dl(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Dim l = aktualaLarĝeco\5
      Dim lLarĝeco = aktualaLarĝeco\5 + 2*_dx
      Return _
         String.Format(
            "h {0} v {1} h -{2} v -{3} h -{4} v {3} h -{2} z m {5} {6} v {7} h {8} v -{7} z m {9} 0 v {7} h {8} v -{7} z",
            aktualaLarĝeco, aktualaAlteco, lLarĝeco, aktualaAlteco - _dy, aktualaLarĝeco - 2*lLarĝeco, _dx, _dy,
            aktualaAlteco - 2*_dy, l, l + _dx*2 + aktualaLarĝeco - 2*lLarĝeco)
   End Function

   Private Function Dr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return _
         String.Format(
            "h {0} v {1} h -{0} z m 0 {1} l {2} {3} h {4} l -{2} -{3} z m {2} 0 v {3} h {4} v -{3} z m {5} 0
h {4} v {3} h -{4} z m 0 {3} l {2} -{3} h {4} l -{2} {3} z",
            aktualaLarĝeco, _dy, aktualaLarĝeco\2 - _dx*2, aktualaAlteco - _dy, _dx, _dx*3)
   End Function

   Private Function S(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return _
         String.Format("h {0} v {1} h -{0} z m 0 {2} l {3} -{2} h {4} l -{3} {2} z m {3} -{2} h {4} l {3} {2} h -{4}",
                       aktualaLarĝeco, _dy, aktualaAlteco, aktualaLarĝeco\2 - _dx\2, _dx)
   End Function

   Private Function R(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return _
         String.Format("h {0} v {1} h -{0} z m 0 {1} l {2} {3} h {4} l -{2} -{3} z m {0} 0 h -{4} l -{2} {3} h {4} z",
                       aktualaLarĝeco, _dy, aktualaLarĝeco\2 - _dx\2,
                       aktualaAlteco - _dy, _dx)
   End Function

   Private Function L(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m {2} {3} v {4} h {5} v -{4} z",
                           aktualaLarĝeco, aktualaAlteco, _dx, _dy, aktualaAlteco - 2*_dy, aktualaLarĝeco - 2*_dx)
   End Function

   Private Function J(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{0} v -{1} h {2} v -{3} h -{2} z",
                           aktualaLarĝeco,
                           _dy,
                           aktualaLarĝeco\2 - _dx\2,
                           aktualaAlteco - _dy*2)
   End Function

   Private Function W(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return _
         String.Format(
            "m {0} 0 a {0} {1} 0 0 0 0 {2} v -{3} a {4} {5} 0 0 1 0 -{6} z a {0} {1} 0 0 1 0 {2} v -{3} a {4} {5} 0 0 0 0 -{6} z",
            aktualaLarĝeco\2, aktualaAlteco\2, aktualaAlteco, _dy, aktualaLarĝeco\2 - _dx, aktualaAlteco\2 - _dy,
            aktualaAlteco - _dy*2)
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
      _vojoj.Add(String.Format("M {0} {1} {2}", x, y, vojo))
   End Sub

   Public Sub Fini()
      _xmlSkribilo.WriteStartDocument()
      _xmlSkribilo.WriteStartElement("svg", "http://www.w3.org/2000/svg")
      _xmlSkribilo.WriteAttributeString("width", Math.Max(_x + _spaco, _dokumentoLarĝeco))
      _xmlSkribilo.WriteAttributeString("height", _dufojaAlteco + _y + 2*_spaco)

      For Each vojo In _vojoj
         _xmlSkribilo.WriteStartElement("path")
         _xmlSkribilo.WriteAttributeString("d", vojo)
         _xmlSkribilo.WriteEndElement()
      Next

      _xmlSkribilo.WriteEndElement()
      _xmlSkribilo.WriteEndDocument()
      _xmlSkribilo.Close()
   End Sub
End Class
