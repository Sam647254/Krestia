Namespace KrestiaInterfaco.Testo
   Public Class TestaVortaro
      Inherits Vortaro
      Private Vortaro = New Dictionary(Of String, Boolean)

      Protected Overrides Async Function EkaldoniKlason(vorto As String, animeco As Boolean) As Task
         Vortaro.Add(vorto, animeco)
         Await Task.CompletedTask
      End Function

      Protected Overrides Async Function EkaldoniVerbo(vorto As String, valenco As Integer) As Task
         Throw New NotImplementedException()
      End Function

      Protected Overrides Async Function EkaldoniPridiranton(vorto As String) As Task
         Throw New NotImplementedException()
      End Function
   End Class
End Namespace