Imports System.Xml

Public Class SVGDesegnilo
   Private ReadOnly xmlSkribilo As XmlWriter
   Private ReadOnly alteco, larĝeco As Integer
   Private ReadOnly dufojaAlteco, duonaLarĝeco As Integer
   Private x, y As Double
   Private ReadOnly dokumentoLarĝeco As Integer
   Private ReadOnly literoDesegniloj As Dictionary(Of String, LiteroDesegnilo) =
      New Dictionary(Of String, LiteroDesegnilo) From {
      {"m", AddressOf M},
      {"p", AddressOf P},
      {"v", AddressOf V},
      {"n", AddressOf N},
      {"t", AddressOf T},
      {"dr", AddressOf Dr},
      {"s", AddressOf S},
      {"l", AddressOf L},
      {"r", AddressOf R},
      {"k", AddressOf K},
      {"h", AddressOf H},
      {"a", AddressOf M},
      {"e", AddressOf E},
      {"i", AddressOf I},
      {"u", AddressOf K},
      {"[", AddressOf NomoKomenco},
      {"]", AddressOf NomoFino},
      {"lokokupilo", AddressOf Lokokupilo},
      {"eco>", AddressOf EcoDekstra},
      {"priskribo", AddressOf Priskribo},
      {"predikativoEstas", AddressOf PredikativoEstas}
   }
   Private ReadOnly vojoj As List(Of String) = New List(Of String)
   Private ReadOnly dx = 4
   Private ReadOnly dy = 10
   Private ReadOnly spaco = 8
   Private ReadOnly spaceto = spaco \ 2

   Delegate Function LiteroDesegnilo(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String

   Public Sub New(elirejo As String, alteco As Integer, larĝeco As Integer,
                  dokumentoLarĝeco As Integer)
      xmlSkribilo = XmlWriter.Create(elirejo)
      Me.alteco = alteco \ 2
      Me.larĝeco = larĝeco + spaceto
      dufojaAlteco = alteco + spaceto
      duonaLarĝeco = larĝeco \ 2
      Me.dokumentoLarĝeco = dokumentoLarĝeco
      x = 10
      y = 10
   End Sub

   Public Sub DesegniSilabon(silabo As String)
      If silabo.Length = 1 Then
         AldoniVojon(literoDesegniloj(silabo)(larĝeco, dufojaAlteco), x, y)
         x += larĝeco + spaceto
      ElseIf silabo.Length = 2 Then
         If ĈuVokalo(silabo.Chars(0)) Then 'VC
            AldoniVojon(literoDesegniloj(silabo.Substring(0, 1))(duonaLarĝeco, dufojaAlteco), x, y)
            x += larĝeco \ 2 + spaceto
            AldoniVojon(literoDesegniloj(silabo.Substring(1, 1))(duonaLarĝeco, dufojaAlteco), x, y)
            x += larĝeco \ 2 + spaceto
         Else 'CV
            AldoniVojon(literoDesegniloj(silabo.Substring(0, 1))(larĝeco, alteco), x, y)
            Dim antaŭY = y
            y += alteco + spaceto
            AldoniVojon(literoDesegniloj(silabo.Substring(1, 1))(larĝeco, alteco), x, y)
            x += larĝeco + spaceto
            y = antaŭY
         End If
      ElseIf silabo.Length = 3 Then
         If silabo.Chars(1) = "r"c Or silabo.Chars(1) = "l"c Then 'CCV
            AldoniVojon(literoDesegniloj(silabo.Substring(0, 2))(larĝeco, alteco), x, y)
            Dim antaŭY = y
            y += alteco + spaceto
            AldoniVojon(literoDesegniloj(silabo.Substring(2, 1))(larĝeco, alteco), x, y)
            x += larĝeco + spaceto
            y = antaŭY
         Else 'CVC
            AldoniVojon(literoDesegniloj(silabo.Substring(0, 1))(larĝeco, alteco), x, y)
            Dim antaŭY = y
            y += alteco + spaceto
            AldoniVojon(literoDesegniloj(silabo.Substring(1, 1))(duonaLarĝeco, alteco), x, y)
            x += larĝeco \ 2 + spaceto
            AldoniVojon(literoDesegniloj(silabo.Substring(2, 1))(duonaLarĝeco, alteco), x, y)
            x += larĝeco \ 2 + spaceto
            y = antaŭY
         End If
      ElseIf silabo.Length = 4 Then 'CCVC
         AldoniVojon(literoDesegniloj(silabo.Substring(0, 2))(larĝeco, alteco), x, y)
         Dim antaŭY = y
         y += alteco + spaceto
         AldoniVojon(literoDesegniloj(silabo.Substring(2, 1))(duonaLarĝeco, alteco), x, y)
         x += larĝeco \ 2 + spaceto
         AldoniVojon(literoDesegniloj(silabo.Substring(3, 1))(duonaLarĝeco, alteco), x, y)
         x += larĝeco \ 2 + spaceto
         y = antaŭY
      End If
   End Sub

   Public Function NomoKomenco() As String
      x += duonaLarĝeco + spaceto
      Return String.Format("h {0} v {1} h -{2} v {3} h {2} v {1} h -{0} z",
                           duonaLarĝeco, dy, duonaLarĝeco - dx, dufojaAlteco - dy * 2)
   End Function

   Public Function NomoFino() As String
      x += duonaLarĝeco + spaceto
      Return String.Format("h {0} v {1} h -{0} v -{2} h {3} v -{4} h -{3} z",
                           duonaLarĝeco, dufojaAlteco, dy, duonaLarĝeco - dx, dufojaAlteco - dy * 2)
   End Function

   Public Function Lokokupilo() As String
      x += duonaLarĝeco + spaceto
      Return String.Format("m 0 {0} h {1} v -{0} h {2} v {0} h {3} v -{0} h {2} v {4} h -{5} z",
                           dufojaAlteco - dy, duonaLarĝeco \ 2 - dx \ 2, dx, duonaLarĝeco \ 2 - dx * 3 \ 2, dufojaAlteco,
                           duonaLarĝeco)
   End Function

   Private Function EcoDekstra() As String
      x += duonaLarĝeco + spaceto
      Return String.Format("m {0} 0 a {0} {1} 0 0 0 0 {2} z v {3} a {4} {5} 0 0 1 0 -{6} z m 0 {7} h -{0} v {8} h {0} z",
                           duonaLarĝeco, alteco, dufojaAlteco, dufojaAlteco - dy, duonaLarĝeco - dx, alteco - dy,
                           dufojaAlteco - 2 * dy,
                           alteco - dy \ 2, dy)
   End Function

   Private Function PredikativoEstas() As String
      x += duonaLarĝeco + spaceto
      Return String.Format("m 0 {3} h {5} v {6} h -{5} z m {1} -{3} h {2} v {4} h -{2} z m {7} 0  h {2} v {4} h -{2} z",
                           dufojaAlteco - dy, duonaLarĝeco \ 2 - dx \ 2, dx, dufojaAlteco \ 2 - dy \ 2, dufojaAlteco,
                           duonaLarĝeco, dy, duonaLarĝeco \ 2)
   End Function

   Private Function Priskribo() As String
      x += duonaLarĝeco + spaceto
      Return String.Format("m 0 {3} h {0} v {1} h -{0} z l {2} -{3} l {2} {3} v {1} l -{2} -{3} l -{2} {3}",
                           duonaLarĝeco, dy, duonaLarĝeco \ 2, dufojaAlteco - dy)
   End Function

   Public Sub DesegniFinaĵo(finaĵo As String)
      Dim antaŭX = x
      Dim antaŭY = y
      AldoniVojon(literoDesegniloj(finaĵo)(0, 0), antaŭX, antaŭY)
   End Sub

   Private Function M(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h {2} v {3} h -{4} v -{5}",
                           dx, aktualaAlteco - dy, aktualaLarĝeco - dx, dy, aktualaLarĝeco, aktualaAlteco)
   End Function

   Private Function P(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} v -{5}",
                           aktualaLarĝeco, dy, aktualaLarĝeco - dx, aktualaAlteco - dy, dx, aktualaAlteco)
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

   Private Function E(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h {2} v {3} h -{2} v {1} h -{0} z",
                           dx, aktualaAlteco \ 2 - dy \ 2, aktualaLarĝeco - dx, dy)
   End Function

   Private Function I(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{2} v {3} h -{4} z",
                           aktualaLarĝeco, dy, aktualaLarĝeco - dx, aktualaAlteco - dy, dx)
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
                           aktualaLarĝeco - dy,
                           dx)
   End Function

   Private Function Dr(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z l {2} {3} v -{3} h {4} v {5} h -{4} l -{2} -{3} z m {0} 0 v {1} l -{2} {3} h -{4} v -{5} h {4} v {3} z",
                           aktualaLarĝeco, dy, aktualaLarĝeco \ 2 - dx * 2, aktualaAlteco - dy, dx, aktualaAlteco, aktualaLarĝeco \ 2 + dx)
   End Function

   Private Function S(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m 0 {2} l {3} -{2} h {4} l -{3} {2} z m {3} -{2} h {4} l {3} {2} h -{4}",
                           aktualaLarĝeco, dy, aktualaAlteco, aktualaLarĝeco \ 2 - dx \ 2, dx)
   End Function

   Private Function R(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z l {2} {3} l {2} -{3} v {1} l -{2} {3} l -{2} -{3} z",
                           aktualaLarĝeco, dy, aktualaLarĝeco \ 2, aktualaAlteco - dy)
   End Function

   Private Function L(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {0} v {1} h -{0} z m {2} {3} v {4} h {5} v -{4} z",
                           aktualaLarĝeco, aktualaAlteco, dx, dy, aktualaAlteco - 2 * dy, aktualaLarĝeco - 2 * dx)
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
      xmlSkribilo.WriteAttributeString("width", x + 10)
      xmlSkribilo.WriteAttributeString("height", "200")

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
