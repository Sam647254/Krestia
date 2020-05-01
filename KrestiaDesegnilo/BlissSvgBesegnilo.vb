Public Class BlissSvgBesegnilo
   Inherits Desegnilo

   Protected Overrides Property LiteroDesegniloj As Dictionary(Of String,LiteroDesegnilo)
   Protected Overrides Property FinaĵoDesegniloj As Dictionary(Of String,FinaĵoDesegnilo)
   
   Public Sub New(elirejo As String, alteco As Integer, larĝeco As Integer, radio As Integer,
                  Optional spaco As Integer = 8)
      MyBase.New(elirejo, alteco, larĝeco, radio, spaco)
   End Sub
End Class