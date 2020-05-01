Imports System.Xml

Public MustInherit Class Desegnilo
   Protected MustOverride Property LiteroDesegniloj As Dictionary(Of String, LiteroDesegnilo)
   Protected MustOverride Property FinaĵoDesegniloj As Dictionary(Of String, FinaĵoDesegnilo)

   Protected Delegate Function LiteroDesegnilo(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String

   Protected Delegate Function FinaĵoDesegnilo() As String
   
   Protected ReadOnly Alteco, Larĝeco As Integer
   Protected ReadOnly DufojaAlteco, DuonaLarĝeco As Integer
   Protected X, Y As Double
   Protected DokumentoLarĝeco As Integer
   Protected ReadOnly Dx As Integer
   Protected ReadOnly Dy As Integer
   Protected ReadOnly Spaco As Integer
   Protected ReadOnly XmlSkribilo As XmlWriter
   
   Private ReadOnly _vojoj As List(Of String) = New List(Of String)
   
   Protected ReadOnly Property Spaceto As Integer
      Get
         Return Spaco/2
      End Get
   End Property
   
   Public Sub New(elirejo As String, alteco As Integer, larĝeco As Integer, dx As Integer, dy As Integer,
                  Optional spaco As Integer = 8)
      XmlSkribilo = XmlWriter.Create(elirejo)
      Me.Alteco = alteco\2
      Me.Larĝeco = larĝeco + Spaceto
      Me.Dx = dx
      Me.Dy = dy
      Me.Spaco = spaco
      DufojaAlteco = alteco + Spaceto
      DuonaLarĝeco = larĝeco\2
      DokumentoLarĝeco = 0
      X = 10
      Y = 10
   End Sub
   
      Public Sub DesegniSilabon(silabo As String)
      If silabo.Length = 1 Then
         AldoniVojon(LiteroDesegniloj(silabo)(Larĝeco, DufojaAlteco), X, Y)
         X += Larĝeco + Spaceto
      ElseIf silabo.Length = 2 Then
         If ĈuVokalo(silabo.Chars(0)) Then 'VC
            AldoniVojon(LiteroDesegniloj(silabo.Substring(0, 1))(DuonaLarĝeco, DufojaAlteco), X, Y)
            X += Larĝeco\2 + Spaceto
            AldoniVojon(LiteroDesegniloj(silabo.Substring(1, 1))(DuonaLarĝeco, DufojaAlteco), X, Y)
            X += Larĝeco\2 + Spaceto
         Else 'CV
            AldoniVojon(LiteroDesegniloj(silabo.Substring(0, 1))(Larĝeco, Alteco), X, Y)
            Dim antaŭY = Y
            Y += Alteco + Spaceto
            AldoniVojon(LiteroDesegniloj(silabo.Substring(1, 1))(Larĝeco, Alteco), X, Y)
            X += Larĝeco + Spaceto
            Y = antaŭY
         End If
      ElseIf silabo.Length = 3 Then
         If silabo.Chars(1) = "r"c Or silabo.Chars(1) = "l"c Then 'CCV
            AldoniVojon(LiteroDesegniloj(silabo.Substring(0, 2))(Larĝeco, Alteco), X, Y)
            Dim antaŭY = Y
            Y += Alteco + Spaceto
            AldoniVojon(LiteroDesegniloj(silabo.Substring(2, 1))(Larĝeco, Alteco), X, Y)
            X += Larĝeco + Spaceto
            Y = antaŭY
         Else 'CVC
            AldoniVojon(LiteroDesegniloj(silabo.Substring(0, 1))(Larĝeco, Alteco), X, Y)
            Dim antaŭY = Y
            Y += Alteco + Spaceto
            AldoniVojon(LiteroDesegniloj(silabo.Substring(1, 1))(DuonaLarĝeco, Alteco), X, Y)
            X += Larĝeco\2 + Spaceto
            AldoniVojon(LiteroDesegniloj(silabo.Substring(2, 1))(DuonaLarĝeco, Alteco), X, Y)
            X += Larĝeco\2 + Spaceto
            Y = antaŭY
         End If
      ElseIf silabo.Length = 4 Then 'CCVC
         AldoniVojon(LiteroDesegniloj(silabo.Substring(0, 2))(Larĝeco, Alteco), X, Y)
         Dim antaŭY = Y
         Y += Alteco + Spaceto
         AldoniVojon(LiteroDesegniloj(silabo.Substring(2, 1))(DuonaLarĝeco, Alteco), X, Y)
         X += Larĝeco\2 + Spaceto
         AldoniVojon(LiteroDesegniloj(silabo.Substring(3, 1))(DuonaLarĝeco, Alteco), X, Y)
         X += Larĝeco\2 + Spaceto
         Y = antaŭY
      Else
         Throw New Exception($"Nevalida silabo: {silabo}")
      End If
   End Sub
   
   Private Sub AldoniVojon(vojo As String, x As Integer, y As Integer)
      _vojoj.Add(String.Format("M {0} {1} {2}", x, y, vojo))
   End Sub
   
   Public Sub DesegniFinaĵon(finaĵo As String)
      Dim antaŭX = X
      Dim antaŭY = Y
      AldoniVojon(FinaĵoDesegniloj(finaĵo)(), antaŭX, antaŭY)
   End Sub
   
   Public Overridable Sub Fini()
      XmlSkribilo.WriteStartDocument()
      XmlSkribilo.WriteStartElement("svg", "http://www.w3.org/2000/svg")
      XmlSkribilo.WriteAttributeString("width", Math.Max(X + Spaco, DokumentoLarĝeco))
      XmlSkribilo.WriteAttributeString("height", DufojaAlteco + Y + 2*Spaco)

      For Each vojo In _vojoj
         XmlSkribilo.WriteStartElement("path")
         XmlSkribilo.WriteAttributeString("d", vojo)
         XmlSkribilo.WriteEndElement()
      Next

      XmlSkribilo.WriteEndElement()
      XmlSkribilo.WriteEndDocument()
      XmlSkribilo.Close()
   End Sub
   
   Private Shared Function ĈuVokalo(litero As Char) As Boolean
      Return litero = "a"c Or
             litero = "e"c Or
             litero = "i"c Or
             litero = "o"c Or
             litero = "u"c Or
             litero = "ɒ"c
   End Function
End Class
