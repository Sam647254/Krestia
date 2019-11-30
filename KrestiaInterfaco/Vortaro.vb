Public MustInherit Class Vortaro
   Public Async Function AldoniKlason(vorto As String, animeco As Boolean) As Task
      If ĈuKlasoInfinitivo(vorto) Then
         Await EkaldoniKlason(vorto, animeco)
      Else
         Throw New InvalidOperationException($"{vorto} ne estas valida infinitivo de klaso")
      End If
   End Function

   Public Sub AldoniVerbon(vorto As String, valenco As Integer)

   End Sub

   Public Sub AldoniPridiranto(vorto As String)

   End Sub

   Protected MustOverride Async Function EkaldoniKlason(vorto As String, animeco As Boolean) As Task
   Protected MustOverride Async Function EkaldoniVerbo(vorto As String, valenco As Integer) As Task
   Protected MustOverride Async Function EkaldoniPridiranton(vorto As String) As Task
End Class
