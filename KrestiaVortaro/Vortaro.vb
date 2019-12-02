Public MustInherit Class Vortaro
   Public Async Function AldoniKlason(vorto As String, animeco As Boolean) As Task
      If ĈuKlasoInfinitivo(vorto) Then
         Await EkaldoniKlason(vorto, animeco)
      Else
         Throw New InvalidOperationException($"{vorto} ne estas valida infinitivo de klaso")
      End If
   End Function

   Public Async Sub AldoniVerbon(vorto As String, valenco As Integer)
      If ĈuVerboInfinitivo(vorto) Then
         Await EkaldoniVerbon(vorto, valenco)
      Else
         Throw New InvalidOperationException($"{vorto} ne estas valida infinitivo de verbo")
      End If
   End Sub

   Public Async Sub AldoniPridiranto(vorto As String)
      If ĈuVerboInfinitivo(vorto) Then
         Await EkaldoniPridiranton(vorto)
      Else
         Throw New InvalidOperationException($"{vorto} ne estas valida infinitivo de verbo")
      End If
   End Sub

   Public MustOverride Async Function Kvanto() As Task(Of Long)

   Protected MustOverride Async Function EkaldoniKlason(vorto As String, animeco As Boolean) As Task
   Protected MustOverride Async Function EkaldoniVerbon(vorto As String, valenco As Integer) As Task
   Protected MustOverride Async Function EkaldoniPridiranton(vorto As String) As Task
End Class