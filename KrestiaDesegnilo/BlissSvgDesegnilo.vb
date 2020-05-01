Imports System.IO
Imports System.Xml
Imports Amazon
Imports Amazon.S3

Public Class BlissSvgDesegnilo
   Inherits Desegnilo

   Protected Overrides Property LiteroDesegniloj As Dictionary(Of String,LiteroDesegnilo)
   Protected Overrides Property FinaĵoDesegniloj As Dictionary(Of String,FinaĵoDesegnilo)
   Private ReadOnly _awsAlirilo As AmazonS3Client
   Private ReadOnly _xmlDokumento As XDocument
   Private ReadOnly _elirejo As String

   Public Sub New(elirejo As String, radio As Integer, Optional spaco As Integer = 8)
      MyBase.New(elirejo, Alteco, Larĝeco, radio, spaco)
      _elirejo = elirejo
      _awsAlirilo = New AmazonS3Client(RegionEndpoint.USWest2)
      _xmlDokumento = New XDocument()
   End Sub

   Public Overrides Sub Fini()
      _xmlDokumento.Save(_elirejo)
   End Sub
   
   Public Sub Desegni(id As Integer)
      Dim bliss = TroviBlissimbolon(id)
      Console.Write(bliss)
   End Sub

   Private Function TroviBlissimbolon(id As Integer) As XElement
      Dim respondo = _awsAlirilo.GetObjectAsync(Bucket, $"bliss_svg_id/{id}.svg").Result
      Using stream = respondo.ResponseStream
         Dim xml = XElement.Load(stream)
         Return xml.LastNode()
      End Using
   End Function

   Private Shadows Const Alteco = 258 - 138
   Private Shadows Const Larĝeco = Alteco\2
   Private Const Bucket = "blissimboloj"
End Class