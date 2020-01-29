using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Amazon.DynamoDBv2;
using Amazon.DynamoDBv2.Model;
using KrestiaVortilo;
using MoreLinq.Extensions;

namespace KrestiaAWSAlirilo {
   public class AwsAlirilo {
      private const string TableName = "Krestia-vortaro-2";

      private readonly AmazonDynamoDBClient _amazonDynamoDbClient =
         new AmazonDynamoDBClient();

      public async Task<IEnumerable<VortoRespondo>> AlportiĈiujnVortojn() {
         var rezulto = await _amazonDynamoDbClient.ScanAsync(new ScanRequest {
            TableName = TableName,
            ProjectionExpression = "vorto, signifo, radikoj, kategorio, noto"
         });
         if (rezulto.LastEvaluatedKey.Count > 0) {
            throw new NotSupportedException("Pli da rezultoj restantaj");
         }

         return rezulto.Items.Select(vorto => new VortoRespondo {
            Vorto = vorto["vorto"].S,
            Signifo = vorto["signifo"].S,
            Kategorioj = vorto.GetValueOrDefault("kategorio")?.SS,
            Noto = vorto.GetValueOrDefault("noto")?.S,
            Radikoj = vorto.GetValueOrDefault("radikoj").L.Select(r => r.S).ToList()
         });
      }

      public async Task<List<ValueTuple<string, string>>> AlportiĈiujnVortojnKunSignifoj() {
         var rezulto = await _amazonDynamoDbClient.ScanAsync(TableName, new List<string> {"vorto", "signifo"});
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

      public async Task<VortoRespondo?> AlportiVorton(string vorto) {
         var respondo = await _amazonDynamoDbClient.GetItemAsync(TableName,
            new Dictionary<string, AttributeValue> {{"vorto", new AttributeValue(vorto)}});
         if (!respondo.IsItemSet) {
            return null;
         }

         var vortoObjecto = respondo.Item;
         return !respondo.IsItemSet
            ? null
            : new VortoRespondo {
               Vorto = vortoObjecto["vorto"].S,
               Kategorioj = vortoObjecto.GetValueOrDefault("kategorio")?.SS,
               Noto = vortoObjecto.GetValueOrDefault("noto")?.S,
               Radikoj = vortoObjecto.GetValueOrDefault("radikoj")?.SS,
               Signifo = vortoObjecto.GetValueOrDefault("signifo")?.S
            };
      }

      public async Task AldoniVortojn(string eniro) {
         var vortoj = (await File.ReadAllLinesAsync(eniro)).Select(vico => {
            var partoj = vico.Split('|');
            if (!Sintaksanalizilo.ĉuInfinitivoB(partoj[0])) {
               throw new ArgumentException($"{partoj[0]} ne estas valida infinitivo");
            }

            return new VortoRespondo {
               Vorto = partoj[0],
               Signifo = partoj[1],
               Kategorioj = partoj[2].Length > 0 ? partoj[2].Split(',').ToList() : null,
               Radikoj = partoj[3].Length > 0 ? partoj[3].Split(',').ToList() : null,
               Noto = partoj[4].Length > 0 ? partoj[4] : null
            };
         });
         await Task.WhenAll(vortoj.Select(vorto => {
            var peto = new Dictionary<string, AttributeValue> {
               {"vorto", new AttributeValue(vorto.Vorto)}, {
                  "bazo",
                  new AttributeValue(Sintaksanalizilo.ĉuVerboInfinitivoB(vorto.Vorto)
                     ? vorto.Vorto.Substring(0, vorto.Vorto.Length - 1)
                     : vorto.Vorto)
               },
               {"signifo", new AttributeValue(vorto.Signifo)},
               {"kategorioj", new AttributeValue(vorto.Kategorioj)},
               {"radikoj", new AttributeValue(vorto.Radikoj)}
            };
            if (vorto.Noto.Length > 0) {
               peto["noto"] = new AttributeValue(vorto.Noto);
            }

            return Task.Run(async () => {
               try {
                  return await _amazonDynamoDbClient.PutItemAsync(new PutItemRequest(TableName, peto) {
                     ConditionExpression = "attribute_not_exists(bazo)"
                  });
               }
               catch (Exception e) {
                  Console.WriteLine($"Ne povis aldoni {vorto.Vorto}");
                  Console.WriteLine(e);
                  throw;
               }
            });
         }));
      }
   }
}