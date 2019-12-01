Imports Amazon.DynamoDBv2

Public Class AWSVortaro
   Inherits Vortaro
   Private client As AmazonDynamoDBClient = New AmazonDynamoDBClient()

   Sub New()

   End Sub

   Protected Overrides Function EkaldoniKlason(vorto As String, animeco As Boolean) As Task
      Throw New NotImplementedException()
   End Function

   Protected Overrides Function EkaldoniVerbo(vorto As String, valenco As Integer) As Task
      Throw New NotImplementedException()
   End Function

   Protected Overrides Function EkaldoniPridiranton(vorto As String) As Task
      Throw New NotImplementedException()
   End Function
End Class
