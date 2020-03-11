﻿Imports System
Imports System.IO

Module Program
   Sub Desegni(svg As SVGDesegnilo, partoj As String())
      For Each silabo In partoj
         Try
            svg.DesegniFinaĵon(silabo)
         Catch ex As Exception
            svg.DesegniSilabon(silabo)
         End Try
      Next
      svg.Fini()
   End Sub

   Sub Main(args As String())
      If args(0) = "testo" Then
         Literoj(args(1))
      Else
         Dosiero(args(1), args(0), Integer.Parse(args(2)), Integer.Parse(args(3)))
      End If
   End Sub

   Sub Dosiero(eliro As String, eniro As String, dx As Integer, dy As Integer)
      Dim svg = New SVGDesegnilo(eliro, 100, 40, dx, dy)
      For Each line In File.ReadLines(eniro)
         Dim silaboj = line.Split(" "c)
         For Each silabo In silaboj
            Try
               svg.DesegniFinaĵon(silabo)
            Catch ex As Exception
               svg.DesegniSilabon(silabo)
            End Try
         Next
         svg.DesegniFinaĵon("vico")
      Next
      svg.Fini()
   End Sub

   Sub Literoj(eliro As String)
      Dim svg = New SVGDesegnilo(eliro, alteco:=100, larĝeco:=40, dx:=4, dy:=10)
      Dim silaboj = New String() {
           "pa", "ba", "ma", "vico",
           "va", "vico",
           "ta", "da", "na", "sa", "la", "ra", "vico",
           "ja", "vico",
           "ka", "ga", "wa", "vico",
           "ha", "vico",
           "mi", "me", "ma", "mu", "mo", "mɒ", "vico",
           "a", "ma", "am", "mam", "vico",
           "[", "kres", "ti", "a", "]", "[", "ti", "me", "ran", "]", "vico",
           "klaso", "rekordo<", "rekordo>", "eco<", "eco>", "nombrigeblaEco<", "nombrigeblaEco>",
           "malplenaVerbo", "netransitiva1", "partaTransitiva1", "partaNetransitiva",
           "transitiva2", "netransitiva2", "partaTransitiva2", "transitiva3",
           "pridiranto", "lokokupilo", "modifanto<", "modifanto>", "vico",
           "nekonitaNombro", "unuNombro", "pluraNombro", "havaĵo",
           "progresivo", "perfekto", "estonteco", "imperativo", "volo1", "volo2", "volo3",
           "atributivoEsti<", "atributivoEsti>", "predikativoEsti", "sola", "ekzistado", "havado",
           "invito", "aganto", "patiento", "translativo", "ĝerundo", "partaNominativo", "partaAkuzativo",
           "partaDativo", "igo", "etigo", "vico",
           "pla", "pra", "bla", "bra", "tla", "tra", "dra", "dla", "kla", "kra", "gla", "gra"
           }
      Desegni(svg, silaboj)
   End Sub
End Module
