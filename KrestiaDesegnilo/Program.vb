Imports System
Imports System.IO

Public Module Program
   Private Sub Desegni(svg As RektangulaSvgDesegnilo, partoj As String())
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
      ElseIf args(0) = "bliss" Then
         Bliss(args(1), args(2), args(3))
      Else
         Dosiero(args(1), args(0), Integer.Parse(args(2)), Integer.Parse(args(3)), 40, 100)
      End If
   End Sub

   Private Sub Bliss(eniro As String, eliro As String, radio As Integer)
      Dim bliss = New BlissSvgDesegnilo(eliro, radio)
      For Each vico In File.ReadLines(eniro)
         For Each silabo In vico.Split(" "c)
            Dim blissId As Integer
            Dim ĉuNumero = Integer.TryParse(silabo, blissId)
            If ĉuNumero
               Console.WriteLine($"Desegnas {blissId}")
               bliss.Desegni(blissId)
            Else
               Try
                  bliss.DesegniFinaĵon(silabo)
               Catch ex As Exception
                  bliss.DesegniSilabon(silabo)
               End Try
            End If
         Next
         bliss.Vico()
      Next
      bliss.Fini()
   End Sub

   Public Sub Dosiero(eliro As String, eniro As IEnumerable(Of String), dx As Integer, dy As Integer, larĝeco As Integer,
                      alteco As Integer)
      Dim svg = New RektangulaSvgDesegnilo(eliro, larĝeco, alteco, dx, dy)
      For Each line In eniro
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

   Private Sub Literoj(eliro As String)
      Dim svg = New RektangulaSvgDesegnilo(eliro, alteco := 100, larĝeco := 40, dx := 4, dy := 10)
      Dim silaboj = New String() { _
                                    "pa", "ba", "ma", "vico",
                                    "va", "vico",
                                    "ta", "da", "na", "sa", "la", "ra", "vico",
                                    "ja", "vico",
                                    "ka", "ga", "wa", "vico",
                                    "ha", "vico",
                                    "mi", "me", "ma", "mu", "mo", "mɒ", "vico",
                                    "a", "ma", "am", "mam", "vico",
                                    "[", "kres", "ti", "a", "]", "[", "ti", "me", "ran", "]", "vico",
                                    "klaso", "rekordo<", "rekordo>", "eco<", "eco>", "nombrigeblaEco<",
                                    "nombrigeblaEco>",
                                    "malplenaVerbo", "netransitiva", "oblikaNetransitiva", "nedirektaNetransitiva",
                                    "transitiva", "nedirektaTransitiva", "oblikaTransitiva", "dutransitiva",
                                    "pridiranto", "lokokupilo", "modifanto<", "modifanto>", "vico",
                                    "difinito", "unuNombro", "pluraNombro", "havaĵo",
                                    "progresivo", "perfekto", "estonteco", "imperativo", "volo1", "volo2", "volo3",
                                    "atributivoEsti<", "atributivoEsti>", "predikativoEsti", "sola", "ekzistado",
                                    "havado",
                                    "invito", "argumento1", "argumento2", "argumento3", "ĝerundo", "ujo1Unue",
                                    "ujo2Unue",
                                    "ujo3Unue", "igo", "etigo", "vico",
                                    "pla", "pra", "bla", "bra", "tla", "tra", "dra", "dla", "kla", "kra", "gla", "gra"
                                 }
      Desegni(svg, silaboj)
   End Sub
End Module
