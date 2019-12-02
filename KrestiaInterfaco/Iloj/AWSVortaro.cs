using Amazon.DynamoDBv2;
using Amazon.DynamoDBv2.Model;
using KrestiaVortaro;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace KrestiaInterfaco.Iloj {
   class AWSVortaro : Vortaro {
      private readonly AmazonDynamoDBClient client = new AmazonDynamoDBClient(Sekretoj.AWSCredentials);
      private const string tableName = "Krestia-vortaro";

      protected override async Task EkaldoniKlason(string vorto, bool animeco) {
         await client.PutItemAsync(tableName, new Dictionary<string, AttributeValue>() {
            { "vorto", new AttributeValue(vorto) },
            { "animeco", new AttributeValue() { BOOL = animeco } }
         });
      }

      protected override async Task EkaldoniPridiranton(string vorto) {
         await client.PutItemAsync(tableName, new Dictionary<string, AttributeValue>() {
            { "vorto", new AttributeValue(vorto) },
         });
      }

      protected override async Task EkaldoniVerbon(string vorto, int valenco) {
         await client.PutItemAsync(tableName, new Dictionary<string, AttributeValue>() {
            { "vorto", new AttributeValue(vorto) },
            { "valenco", new AttributeValue() { N = valenco.ToString() } }
         });
      }

      public override async Task<long> Kvanto() {
         return (await client.DescribeTableAsync(tableName)).Table.ItemCount;
      }
   }
}
