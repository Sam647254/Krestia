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

      public async Task AldoniPlurajn(IEnumerable<string> vicoj) {
         var novaVortoj = vicoj.Select(vico => {
            var partoj = vico.Split("|");
            var vorto = partoj[0];
            var valencoAŭAnimeco = partoj[1];
            var signifo = partoj[2];
            var radikoj = partoj[3];

            return vorto switch {
               string v when Kontrolilaro.ĈuKlasoInfinitivo(v) =>
                  new WriteRequest(new PutRequest(new Dictionary<string, AttributeValue>() {
                     { "vorto", new AttributeValue(v) },
                     { "animeco", new AttributeValue() { BOOL = valencoAŭAnimeco == "T" } },
                     { "signifo", new AttributeValue(signifo) },
                     { "radikoj", ListiRadikojn(radikoj) }
                  })),
               string v when Kontrolilaro.ĈuVerboInfinitivo(v) =>
                  new WriteRequest(new PutRequest(new Dictionary<string, AttributeValue>() {
                     { "vorto", new AttributeValue(v) },
                     { "valenco", new AttributeValue() { N = valencoAŭAnimeco } },
                     { "signifo", new AttributeValue(signifo) },
                     { "radikoj", ListiRadikojn(radikoj) }
                  })),
               string v when Kontrolilaro.ĈuPridiranto(v) =>
                  new WriteRequest(new PutRequest(new Dictionary<string, AttributeValue>() {
                     { "vorto", new AttributeValue(v) },
                     { "signifo", new AttributeValue(signifo) },
                     { "radikoj", ListiRadikojn(radikoj) }
                  })),
               _ => throw new InvalidOperationException($"Nekonita vorto {vorto}")
            };
         });
         await Task.Run(() => {
            novaVortoj.Batch(25).ForEach(async grupo => {
               await client.BatchWriteItemAsync(new Dictionary<string, List<WriteRequest>>() {
                  { tableName, grupo.ToList() }
               });
            });
         });
      }

      private AttributeValue ListiRadikojn(string radikoj) {
         return new AttributeValue() {
            L = radikoj.Split(",").Where(r => r.Length > 0).Select(r => new AttributeValue(r)).ToList(),
            IsLSet = true
         };
      }
   }
}
