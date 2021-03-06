﻿Imports <xmlns="http://www.w3.org/2000/svg">
Imports Amazon
Imports Amazon.S3

Public Class BlissSvgDesegnilo
   Inherits Desegnilo

   Private Property XMax As Integer

   Protected Overrides Property LiteroDesegniloj As Dictionary(Of String,LiteroDesegnilo) =
      New Dictionary(Of String, LiteroDesegnilo) From {
         {"p", AddressOf P},
         {"b", Function(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
            Return String.Format("m 0 {0} v -{0} h {1} m -{2} 0 v {0}", aktualaAlteco, aktualaLarĝeco, aktualaLarĝeco\2)
         End Function},
         {"m", AddressOf M},
         {"v", Function(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
            Return _
               String.Format("m {0} 0 h -{0} l {1} {2}", aktualaLarĝeco,
                             If(aktualaLarĝeco >= Larĝeco, aktualaLarĝeco\2, aktualaLarĝeco), aktualaAlteco)
         End Function},
         {"t", Function(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
            Return String.Format("h {1} m -{2} 0 v {0}", aktualaAlteco, aktualaLarĝeco, aktualaLarĝeco\2)
         End Function},
         {"d", Function(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
            Return _
               String.Format("h {1} m -{2} 0 v {0} m {3} -{0} v {0}", aktualaAlteco, aktualaLarĝeco, aktualaLarĝeco*2\3,
                             aktualaLarĝeco\3)
         End Function},
         {"n", Function(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
            Return String.Format("m 0 {0} h {1} m -{2} -{0} v {0}", aktualaAlteco, aktualaLarĝeco, aktualaLarĝeco\2)
         End Function},
         {"s", function(aktualaLarĝeco, aktualaAlteco) 
            Return _
               String.Format("h {0} m -{0} {1} l {2} -{1} l {2} {1}", aktualaLarĝeco, aktualaAlteco, aktualaLarĝeco\2)
         End function},
         {"l", Function(aktualaLarĝeco, aktualaAlteco)
            Return String.Format("h {0} v {1} h -{0} z", aktualaLarĝeco, aktualaAlteco)
         End function},
         {"r", Function(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
            Return String.Format("h {0} l -{1} {2} z", aktualaLarĝeco, aktualaLarĝeco\2, aktualaAlteco)
         End Function},
         {"ʃ", Function(aktualaLarĝeco, aktualaAlteco) As String 
            Return String.Format("h {0} v {1} m -{0} 0 l {0} -{1}", aktualaLarĝeco, aktualaAlteco)
         End function},
         {"k", Addressof K},
         {"h", function(aktualaLarĝeco, aktualaAlteco) 
            Return String.Format("h {0} m -{0} {1} h {0}", aktualaLarĝeco, aktualaAlteco)
         End function},
         {"tl", function(aktualaLarĝeco, aktualaAlteco) 
            Return _
               String.Format("h {0} m -{1} 0 v {3} h {2} v -{3}", aktualaLarĝeco, aktualaLarĝeco*2\3, aktualaLarĝeco\3,
                             aktualaAlteco)
         End function},
         {"tr", function(aktualaLarĝeco, aktualaAlteco) 
            Return _
               String.Format("h {0} m -{1} 0 l {2} {3} l {2} -{3}", aktualaLarĝeco, aktualaLarĝeco*2\3, aktualaLarĝeco\6,
                             aktualaAlteco)
         End function},
         {"kr", function(aktualaLarĝeco, aktualaAlteco)
            Return _
               String.Format("m {0} 0 l -{1} {2} l -{1} -{2} h {3} v {2}", aktualaLarĝeco\2, aktualaLarĝeco\4,
                             aktualaAlteco, aktualaLarĝeco)
         End function},
         {"gr", function(aktualaLarĝeco, aktualaAlteco)
            Return String.Format("m {0} 0 l -{1} {2} l -{1} -{2} h {3} v {2} m -{4} -{2} v {2}",
                                 aktualaLarĝeco\2, aktualaLarĝeco\4,
                                 aktualaAlteco, aktualaLarĝeco, aktualaLarĝeco\3)
         End function},
         {"i", AddressOf P},
         {"e", Function(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
            Return String.Format("v {0} m 0 -{1} h {2}", aktualaAlteco, aktualaAlteco\2, aktualaLarĝeco)
         End Function},
         {"a", AddressOf M},
         {"u", AddressOf K},
         {"o", Function(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
            Return String.Format("m 0 {1} h {2} m 0 -{1} v {0}", aktualaAlteco, aktualaAlteco\2, aktualaLarĝeco)
         End Function},
         {"ɒ", Function(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
            Return String.Format("m 0 {0} h {1} v -{0}", aktualaAlteco, aktualaLarĝeco)
         End Function},
         {"x", Function(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
            Return String.Format("m 0 {0} l {1} -{0} l {1} {0}", aktualaAlteco, aktualaLarĝeco\2)
         End Function}
         }

   Protected Overrides Property FinaĵoDesegniloj As Dictionary(Of String,FinaĵoDesegnilo) =
      New Dictionary(Of String, FinaĵoDesegnilo) From {
         {"[", AddressOf NomoKomenco},
         {"]", Function() As String
            X += DuonaLarĝeco + Spaco
            Return String.Format("h {0} v {1} h -{0}", DuonaLarĝeco, DufojaAlteco)
         End Function},
         {"lokokupilo", Function() As String
            X += DuonaLarĝeco + Spaco
            Return String.Format("m 0 {0} h {1} v -{0} m -{2} {0} v -{0}", DufojaAlteco, DuonaLarĝeco, DuonaLarĝeco\2)
         End Function},
         {"nombrigeblaKlaso", AddressOf Klaso},
         {"nenombrigeblaKlaso", AddressOf Klaso},
         {"antaŭNenombrigeblaEco", Function() As String
            X += DuonaLarĝeco + Spaco
            Return _
               String.Format("m {0} 0 a {0} {1} 0 0 0 0 {2} m -{0} -{3} h {0}", DuonaLarĝeco, DufojaAlteco\2,
                             DufojaAlteco, DufojaAlteco\2)
         End Function},
         {"malantaŭNenombrigeblaEco", Function() As String
            X += DuonaLarĝeco + Spaco
            Return _
               String.Format("a {0} {1} 0 0 1 0 {2} m 0 -{3} h {0}", DuonaLarĝeco, DufojaAlteco\2,
                             DufojaAlteco, DufojaAlteco\2)
         End Function},
         {"antaŭNombrigeblaEco", function()
            X += DuonaLarĝeco + Spaco
            Return _
               String.Format("m {0} 0 h -{0} v {1} h {0} m -{0} -{2} h {0}", DuonaLarĝeco, DufojaAlteco, DufojaAlteco\2)
         End Function},
         {"rekordo<", Function() As String
            X += DuonaLarĝeco + Spaceto + Spaco
            Return _
               String.Format("a {0} {1} 0 0 1 0 {2} m {3} -{2} v {4} m 0 {5} v {4}", DuonaLarĝeco, DufojaAlteco\2,
                             DufojaAlteco,
                             DuonaLarĝeco + Spaceto, Alteco, Spaceto)
         End Function},
         {"rekordo>", Function() As String
            X += DuonaLarĝeco + Spaceto + Spaco
            Return _
               String.Format("m {0} 0 a {0} {1} 0 0 0 0 {2} m {4} -{2} v {3} m 0 {4} v {3}", DuonaLarĝeco,
                             DufojaAlteco\2,
                             DufojaAlteco, Alteco, Spaceto)
         End Function},
         {"pridiranto", Function() As String
            X += DuonaLarĝeco + Spaco
            Return String.Format("m {0} 0 l -{0} {1} h {2} z", DuonaLarĝeco\2, DufojaAlteco, DuonaLarĝeco)
         End Function},
         {"atributivoEstiMalantaŭ", Function() As String
            X += DuonaLarĝeco + Spaco
            Return String.Format("m 0 {0} h {1} m -{1} {0} h {1} m 0 -{2} v {3}", DufojaAlteco\3,
                                 DuonaLarĝeco, DufojaAlteco*2\3, DufojaAlteco)
         End Function},
         {"atributivoEstiAntaŭ", Function() As String
            X += DuonaLarĝeco + Spaco
            Return String.Format("v {3} m 0 -{2} h {1} m -{1} {0} h {1}", DufojaAlteco\3,
                                 DuonaLarĝeco, DufojaAlteco*2\3, DufojaAlteco)
         End Function},
         {"predikativoEsti", Function() As String
            X += DuonaLarĝeco + Spaco
            Return _
               String.Format("m 0 {0} h {1} m -{2} -{0} v {3} m {2} -{3} v {3}", DufojaAlteco\2, DuonaLarĝeco,
                             DuonaLarĝeco\2, DufojaAlteco)
         End Function},
         {"netransitivaVerbo", Function() As String
            X += DuonaLarĝeco + Spaco
            Return _
               String.Format("m 0 {0} h {1} m -{2} -{0} v {3}", DufojaAlteco\2, DuonaLarĝeco, DuonaLarĝeco\2,
                             DufojaAlteco)
         End Function},
         {"nedirektaTransitivaVerbo", Function() As String
            X += DuonaLarĝeco + Spaco
            Return String.Format("h {0} m -{1} 0 v {2} m -{1} 0 h {0}", DuonaLarĝeco, DuonaLarĝeco\2, DufojaAlteco)
         End Function},
         {"transitivaVerbo", function() 
            X += DuonaLarĝeco + Spaco
            Return String.Format("h {0} l -{0} {1} h {0}", DuonaLarĝeco, DufojaAlteco)
         End function},
         {"oblikaTransitivaVerbo", function()
            X += DuonaLarĝeco + Spaco
            Return String.Format("l {0} {1} l {0} -{1} m -{2} {3} h {2} m -{0} 0 v {1} m -{0} 0 h {2}",
                                 DuonaLarĝeco\2, Alteco, DuonaLarĝeco, Alteco + Spaceto)
         End function},
         {"malantaŭModifanto", function() 
            X += DuonaLarĝeco + Spaco
            Return String.Format("h {0} v {1}", DuonaLarĝeco, DufojaAlteco)
         End function},
         {"antaŭModifanto", Function()
            X += DuonaLarĝeco + Spaco
            Return String.Format("m {0} 0 h -{0} v {1}", DuonaLarĝeco, DufojaAlteco)
         End Function},
         {"difinito", function()
            X += DuonaLarĝeco + Spaco
            Return String.Format("m {0} 0 l -{0} {1} l {0} {1} m 0 -{1} h -{0}", DuonaLarĝeco, DufojaAlteco\2)
         End function},
         {"pluraNombro", Function()
            X += DuonaLarĝeco + Spaco + Spaceto
            Return String.Format("m {0} 0 l -{0} {1} l {0} {1} m -{0} -{1} h {0} m {2} -{1} v {3} m 0 {2} v {3}",
                                 DuonaLarĝeco, DufojaAlteco\2, Spaceto, Alteco)
         End function},
         {"havaĵo", Function()
            X += DuonaLarĝeco
            Return String.Format("m 0 {0} h {1} m 0 -{0} v {2}", DufojaAlteco\2, DuonaLarĝeco, DufojaAlteco)
         End Function},
         {"progresivo", Function()
            X += DuonaLarĝeco + Spaco + Spaco
            Return String.Format("l {0} {1} l -{0} {1} m {2} -{3} l {0} {1} l -{0} {1}",
                                 DuonaLarĝeco, DufojaAlteco\2, Spaco, DufojaAlteco)
         End function},
         {"perfekto", Function()
            X += DuonaLarĝeco + Spaco
            Return String.Format("l {0} {1} l -{0} {1} m {0} -{2} v {2}", DuonaLarĝeco, DufojaAlteco\2, DufojaAlteco)
         End Function},
         {"estonteco", Function()
            X += DuonaLarĝeco + Spaco
            Return String.Format("v {0} l {1} -{2} z", DufojaAlteco, DuonaLarĝeco, DufojaAlteco\2)
         End Function},
         {"sola",Function ()
            X += DuonaLarĝeco + Spaco
            Return _
               String.Format("v {0} m 0 {1} v {0} m {2} -{3} v {0} m 0 {1} v {0}", Alteco, Spaceto, DuonaLarĝeco,
                             DufojaAlteco)
         End Function},
         {"argumento2", Function()
            X += DuonaLarĝeco + Spaco
            Return String.Format("m {0} 0 l -{0} {1} l {0} {1} l {0} -{1} z", DuonaLarĝeco\2, DufojaAlteco\2)
         End Function},
         {"unueUjo2", Function()
            X += DuonaLarĝeco + Spaceto*2 + Spaco
            Return _
               String.Format("m 0 {0} v {0} m {1} -{0} v {0} m {1} -{2} l {3} {0} l -{3} {0}", DufojaAlteco\2, Spaceto,
                             DufojaAlteco, DuonaLarĝeco)
         End Function}
         }

   Private ReadOnly _awsAlirilo As AmazonS3Client
   Private ReadOnly _xmlDokumento As XDocument
   Private ReadOnly _elirejo As String
   Private ReadOnly _blissimboloj As List(Of XElement)

   Private Function NomoKomenco() As String
      X += DuonaLarĝeco + Spaco
      Return String.Format("m {0} 0 h -{0} v {1} h {0}", DuonaLarĝeco, DufojaAlteco)
   End Function

   Private Function P(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("m 0 {0} v -{0} h {1}", aktualaAlteco, aktualaLarĝeco)
   End Function

   Private Function M(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("v {0} h {1}", aktualaAlteco, aktualaLarĝeco)
   End Function

   Private Function K(aktualaLarĝeco As Integer, aktualaAlteco As Integer) As String
      Return String.Format("h {1} v {0}", aktualaAlteco, aktualaLarĝeco)
   End Function

   Private Function Klaso() As String
      X += DuonaLarĝeco + Spaco
      Return String.Format("m {0} 0 l -{0} {1} l {0} {1}", DuonaLarĝeco, DufojaAlteco\2)
   End Function

   Public Sub New(elirejo As String, radio As Integer, Optional spaco As Integer = 22)
      MyBase.New(elirejo, DufojaAlteco - spaco\2 - 5, Larĝeco, radio, radio, spaco, True)
      _elirejo = elirejo
      _blissimboloj = New List(Of XElement)
      _awsAlirilo = New AmazonS3Client(RegionEndpoint.USWest2)
      _xmlDokumento = New XDocument()
      Y = 130
   End Sub

   Public Sub Vico()
      XMax = Math.Max(XMax, X)
      X = 10
      Y += DufojaAlteco + Spaco*4
   End Sub

   Public Overrides Sub Fini()
      Dim svg =
             <svg width=<%= Math.Max(XMax, X) %> height=<%= Y + DufojaAlteco + 2*Spaceto %> stroke-linecap="round"></svg>
      svg.Add(
         <style type="text/css">
         .brush0 { fill: rgb(255,255,255); }
         .pen0 { stroke: rgb(0,0,0); stroke-width: 1; stroke-linejoin: round; }
         .font0 { font-size: 11px; font-family: "MS Sans Serif"; }
         .pen1 { stroke: rgb(0,0,0); stroke-width: <%= Dx %>; stroke-linejoin: round; }
         .brush1 { fill: none; }
         .font1 { font-weight: bold; font-size: 16px; font-family: System, sans-serif; }
         </style>
         )

      For Each vojo In Vojoj
         svg.Add(<path class="pen1" fill="none" d=<%= vojo %> />)
      Next

      For Each simbolo In _blissimboloj
         svg.Add(simbolo)
      Next

      _xmlDokumento.Add(svg)
      _xmlDokumento.Save(_elirejo)
   End Sub

   Public Sub Desegni(id As Integer)
      Dim simboloLarĝeco As Integer
      Dim bliss = TroviBlissimbolon(id, simboloLarĝeco)
      bliss.Add(New XAttribute("transform", $"translate({X}, {Y - 130})"))
      X += simboloLarĝeco + Spaco
      _blissimboloj.Add(bliss)
   End Sub

   Private Function TroviBlissimbolon(id As Integer, ByRef simboloLarĝeco As Integer) As XElement
      Dim respondo = _awsAlirilo.GetObjectAsync(Bucket, $"bliss_svg_id/{id}.svg").Result
      Using stream = respondo.ResponseStream
         Dim xml = XElement.Load(stream)
         simboloLarĝeco = Integer.Parse(xml.Attribute("viewBox").Value.Split(" "c)(2))
         Return xml.LastNode()
      End Using
   End Function

   Private Shadows Const DufojaAlteco = 258 - 130
   Private Shadows Const Larĝeco = DufojaAlteco * 2 / 5

   Private Shadows ReadOnly Property Alteco As Integer
      Get
         Return DufojaAlteco\2 - Spaceto\2 - 5
      End Get
   End Property
   
   Protected Overrides ReadOnly Property Spaceto As Integer
      Get
         Return Dx\2 + 10
      End Get
   End Property


   Private Const Bucket = "blissimboloj"
End Class