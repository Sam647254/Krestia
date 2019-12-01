Imports Amazon.CognitoIdentity
Imports Amazon.DynamoDBv2

Public Class AWSVortaro
   Inherits Vortaro
   Private Shared ReadOnly tableName = "Krestia-vortaro"
   Private ReadOnly client As AmazonDynamoDBClient = New AmazonDynamoDBClient(
      New CognitoAWSCredentials("us-west-2:9be367d6-cafb-4439-ac03-be34b778cba3",
                                Amazon.RegionEndpoint.USWest2))

   Protected Overrides Async Function EkaldoniKlason(vorto As String, animeco As Boolean) As Task
      Await client.PutItemAsync(tableName,
                                  New Dictionary(Of String, Model.AttributeValue) From
                                     {{"vorto", New Model.AttributeValue("animeco") With {.BOOL = animeco}}})
   End Function

   Protected Overrides Function EkaldoniVerbo(vorto As String, valenco As Integer) As Task
      Throw New NotImplementedException()
   End Function

   Protected Overrides Function EkaldoniPridiranton(vorto As String) As Task
      Throw New NotImplementedException()
   End Function
End Class