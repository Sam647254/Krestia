Imports System.Xml

Public Class SVGDesegnilo
   Private ReadOnly xmlSkribilo As XmlWriter

   Public Sub New(elirejo As String)
      xmlSkribilo = XmlWriter.Create(elirejo)
   End Sub

   Public Sub Fini()
      xmlSkribilo.Close()
   End Sub
End Class
