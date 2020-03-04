Imports System
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
      Literoj()
   End Sub

   Sub Dosiero(eliro As String, eniro As String)
      Dim svg = New SVGDesegnilo(eliro, 100, 40)
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

   Sub Literoj()
      Dim svg = New SVGDesegnilo("literoj2.svg", 100, 40)
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

   Sub Valentin()
      Dim svg = New SVGDesegnilo("valentin2.svg", 100, 40)
      Dim silaboj = New String() {
           "mi", "ri", "mi", "transitiva2", "imperativo",
           "te", "mi", "eco>", "nekonitaNombro",
           "[", "va", "len", "tin", "]", "vico",
           "se", "transitiva2", "progresivo",
           "hem", "lokokupilo",
           "me", "ra", "transitiva2", "mankaNominativo", "ĝerundo",
           "hes", "lokokupilo",
           "glo", "re", "eco<", "nekonitaNombro",
           "o", "ve", "modifanto<",
           "wil", "lokokupilo",
           "te", "modifanto<", "vico",
           "kres", "ku", "nekonitaNombro",
           "e", "modifanto<",
           "hes", "lokokupilo",
           "li", "re", "eco<", "pluraNombro",
           "lu", "ta", "netransitiva1", "progresivo",
           "po", "ti", "modifanto<",
           "gre", "li", "pridiranto",
           "wel", "lokokupilo",
           "lus", "tɒ", "atributivoEsti"}
      Desegni(svg, silaboj)
   End Sub

   Sub Test()
      Dim svg = New SVGDesegnilo("test.svg", 100, 40)
      svg.DesegniSilabon("i")
      svg.DesegniSilabon("re")
      svg.DesegniFinaĵon("rekordo<")
      svg.DesegniFinaĵon("predikativoEstas")

      svg.DesegniSilabon("me")
      svg.DesegniSilabon("ra")
      svg.DesegniFinaĵon("eco>")
      svg.DesegniFinaĵon("[")
      svg.DesegniSilabon("me")
      svg.DesegniSilabon("til")
      svg.DesegniSilabon("mo")
      svg.DesegniFinaĵon("]")
      svg.DesegniFinaĵon("vico")

      svg.DesegniSilabon("mo")
      svg.DesegniSilabon("re")
      svg.DesegniFinaĵon("malplenaVerbo")
      svg.DesegniFinaĵon("progresivo")

      svg.DesegniSilabon("lu")
      svg.DesegniSilabon("ve")
      svg.DesegniFinaĵon("malplenaVerbo")
      svg.DesegniFinaĵon("estonteco")
      svg.DesegniSilabon("mor")
      svg.DesegniSilabon("gal")
      svg.DesegniFinaĵon("modifanto")
      svg.DesegniFinaĵon("vico")

      svg.DesegniSilabon("dru")
      svg.DesegniSilabon("re")
      svg.DesegniFinaĵon("eco>")
      svg.DesegniFinaĵon("[")
      svg.DesegniSilabon("liv")
      svg.DesegniSilabon("ra")
      svg.DesegniFinaĵon("]")
      svg.DesegniSilabon("u")
      svg.DesegniSilabon("na")
      svg.DesegniFinaĵon("priskribo")
      svg.DesegniFinaĵon("predikativoEstas")
      svg.DesegniFinaĵon("vico")

      svg.DesegniSilabon("dru")
      svg.DesegniSilabon("re")
      svg.DesegniFinaĵon("eco>")
      svg.DesegniFinaĵon("[")
      svg.DesegniSilabon("mel")
      svg.DesegniSilabon("mir")
      svg.DesegniFinaĵon("]")
      svg.DesegniSilabon("u")
      svg.DesegniSilabon("na")
      svg.DesegniFinaĵon("priskribo")
      svg.DesegniFinaĵon("predikativoEstas")
      svg.DesegniFinaĵon("vico")

      svg.DesegniSilabon("mi")
      svg.DesegniSilabon("ri")
      svg.DesegniSilabon("mi")
      svg.DesegniFinaĵon("transitiva2")
      svg.DesegniFinaĵon("imperativo")
      svg.DesegniSilabon("li")
      svg.DesegniSilabon("tɒ")
      svg.DesegniFinaĵon("nekonitaNombro")
      svg.DesegniFinaĵon("vico")

      svg.DesegniSilabon("li")
      svg.DesegniSilabon("ta")
      svg.DesegniFinaĵon("rekordo>")
      svg.DesegniFinaĵon("sola")

      svg.Fini()
   End Sub
End Module
