Imports System.Xml

Public Class SVGDesegnilo
   Private ReadOnly xmlSkribilo As XmlWriter
   Private ReadOnly alteco As Integer
   Private ReadOnly larĝeco As Integer
   Private ReadOnly documentoLarĝeco As Integer

   Public Sub New(elirejo As String, alteco As Integer, larĝeco As Integer,
                  dokumentoLarĝeco As Integer)
      xmlSkribilo = XmlWriter.Create(elirejo)
      xmlSkribilo.WriteStartDocument()
      Me.alteco = alteco
      Me.larĝeco = larĝeco
   End Sub

   Public Sub Fini()
      xmlSkribilo.WriteEndDocument()
      xmlSkribilo.Close()
   End Sub
End Class
