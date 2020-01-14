using Amazon;
using Amazon.DynamoDBv2;
using Amazon.DynamoDBv2.Model;
using KrestiaVortaro;
using MoreLinq;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace KrestiaInterfaco.Iloj {
   class AWSVortaro : Vortaro {
      private readonly AmazonDynamoDBClient client = new AmazonDynamoDBClient(Sekretoj.AWSCredentials, RegionEndpoint.USWest2);
      private const string tableName = "Krestia-vortaro-2";

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

      public async Task AldoniPlurajn(IEnumerable<string> vicoj) {
         var novaVortoj = vicoj.Select(vico => {
            var partoj = vico.Split("|");
            var vorto = partoj[0];
            var radikoj = partoj[1].Split(',');
            var signifo = partoj[2];
            var noto = partoj.Length >= 4 ? partoj[3] : null;
            var rilataj = partoj.Length == 5 ? partoj[4].Split(',') : null;

            var peto = new Dictionary<string, AttributeValue>() {
               { "vorto", new AttributeValue(vorto) },
               { "signifo", new AttributeValue(signifo) },
               { "radikoj",
                  new AttributeValue() {
                     L = radikoj.Where(r => r.Length > 0).Select(r => new AttributeValue(r)).ToList(),
                     IsLSet = true
                  }
               }
            };
            if (!string.IsNullOrEmpty(noto)) {
               peto["noto"] = new AttributeValue(noto);
            }
            if (rilataj != null) {
               peto["rilataj"] = new AttributeValue() {
                  L = rilataj.Where(r => r.Length > 0).Select(r => new AttributeValue(r)).ToList()
               };
            }

            return new PutItemRequest(tableName, peto);
         });
         await Task.Run(() => {
            novaVortoj.ForEach(async vorto => {
               await client.PutItemAsync(vorto);
            });
         });
      }
   }
}
