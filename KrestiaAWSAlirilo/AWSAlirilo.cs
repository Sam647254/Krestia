using Amazon.DynamoDBv2;

namespace KrestiaAWSAlirilo {
   public class AWSAlirilo {
      private const string tableName = "Krestia-vortaro-2";
      private readonly AmazonDynamoDBClient _amazonDynamoDbClient =
         new AmazonDynamoDBClient();
   }
}