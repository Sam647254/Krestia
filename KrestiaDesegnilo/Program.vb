Imports System

Module Program
   Sub Main(args As String())
      Dim svg = New SVGDesegnilo("test.svg", 100, 40, 200)
      svg.DesegniSilabon("dru")
      svg.DesegniSilabon("re")
      svg.DesegniFinaĵo("eco>")
      svg.DesegniFinaĵo("[")
      svg.DesegniSilabon("liv")
      svg.DesegniSilabon("ra")
      svg.DesegniFinaĵo("]")
      svg.DesegniSilabon("u")
      svg.DesegniSilabon("na")
      svg.DesegniFinaĵo("priskribo")
      svg.DesegniFinaĵo("predikativoEstas")
      svg.Fini()
   End Sub
End Module
