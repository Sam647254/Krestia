using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Amazon.DynamoDBv2;
using Amazon.DynamoDBv2.Model;

namespace KrestiaAWSAlirilo {
   public class AwsAlirilo {
      private const string TableName = "Krestia-vortaro-2";

      private readonly AmazonDynamoDBClient _amazonDynamoDbClient =
         new AmazonDynamoDBClient();

      public async Task<List<ValueTuple<string, string>>> AlportiĈiujnVortojn() {
         var rezulto = await _amazonDynamoDbClient.ScanAsync(TableName, new List<string>() {"vorto", "signifo"});
         if (rezulto.LastEvaluatedKey.Count > 0) {
            throw new NotSupportedException("Pli da rezultoj restantaj");
         }

         return rezulto.Items.Select(r => (r["vorto"].S, r["signifo"].S)).ToList();
      }

      public async Task RedaktiVorton(string vorto, string eco, string valuo) {
         await _amazonDynamoDbClient.UpdateItemAsync(TableName,
            new Dictionary<string, AttributeValue> {{"vorto", new AttributeValue(vorto)}},
            new Dictionary<string, AttributeValueUpdate>
               {{eco, new AttributeValueUpdate(new AttributeValue(valuo), AttributeAction.PUT)}});
      }

      public async Task RedaktiVorton(string vorto, string eco, List<string> valuoj) {
         await _amazonDynamoDbClient.UpdateItemAsync(TableName,
            new Dictionary<string, AttributeValue> {{"vorto", new AttributeValue(vorto)}},
            new Dictionary<string, AttributeValueUpdate>
               {{eco, new AttributeValueUpdate(new AttributeValue(valuoj), AttributeAction.PUT)}});
      }
   }
}