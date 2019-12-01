Namespace KrestiaInterfaco.Testo
   Public Class TestaVortaro
      Inherits Vortaro
      Private ReadOnly Klasoj = New Dictionary(Of String, Boolean)
      Private ReadOnly Verboj = New Dictionary(Of String, Integer)
      Private ReadOnly Pridirantoj = New HashSet(Of String)

      Protected Overrides Async Function EkaldoniKlason(vorto As String, animeco As Boolean) As Task
         Klasoj.Add(vorto, animeco)
         Await Task.CompletedTask
      End Function

      Protected Overrides Async Function EkaldoniVerbo(vorto As String, valenco As Integer) As Task
         Verboj.Add(vorto, valenco)
         Await Task.CompletedTask
      End Function

      Protected Overrides Async Function EkaldoniPridiranton(vorto As String) As Task
         Pridirantoj.Add(vorto)
         Await Task.CompletedTask
      End Function
   End Class
End Namespace