using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using Amazon;
using Amazon.DynamoDBv2;
using Amazon.DynamoDBv2.Model;
using KrestiaVortilo;
using MoreLinq.Extensions;

namespace KrestiaAWSAlirilo {
   public class AwsAlirilo {
      private const string TableName = "Krestia-vortaro-2";

      private readonly AmazonDynamoDBClient _amazonDynamoDbClient = new AmazonDynamoDBClient(RegionEndpoint.USWest2);

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

         var vorttipo = Sintaksanalizilo.infinitivoNomoDe(vorto).Value;
         var silaboj = Sintaksanalizilo2.dividiKunFinaĵo(vorto);

         if (silaboj.IsError) {
            throw new Exception(silaboj.ErrorValue);
         }

         var vortoObjecto = respondo.Item;
         return !respondo.IsItemSet
            ? null
            : new VortoRespondo {
               Vorto = vortoObjecto["vorto"].S,
               Kategorioj = vortoObjecto.GetValueOrDefault("kategorioj")?.SS,
               Noto = vortoObjecto.GetValueOrDefault("noto")?.S,
               Radikoj = vortoObjecto.GetValueOrDefault("radikoj")?.SS,
               Signifo = vortoObjecto.GetValueOrDefault("signifo")?.S,
               Vorttipo = vorttipo,
               Silaboj = silaboj.ResultValue
            };
      }

      public async Task AldoniVortojn(string eniro) {
         var vortoj = (await File.ReadAllLinesAsync(eniro)).Select(vico => {
            var partoj = vico.Split('|');
            if (partoj.Length != 5) {
               throw new ArgumentException($"{vico} estas nevalida");
            }
            if (!Sintaksanalizilo2.ĉuInfinitivoB(partoj[0])) {
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
                  new AttributeValue(Sintaksanalizilo2.bazoDe(vorto.Vorto))
               },
               {"signifo", new AttributeValue(vorto.Signifo)}
            };
            if (vorto.Kategorioj.Count > 0) {
               peto["kategorioj"] = new AttributeValue(vorto.Kategorioj);
            }

            if (vorto.Radikoj.Count > 0) {
               peto["radikoj"] = new AttributeValue(vorto.Radikoj);
            }

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
                  Console.WriteLine($"Ne povis aldoni {vorto.Vorto}: {e.Message}");
                  return null;
               }
            });
         }));
      }

      public async Task<VortoRezulto> TroviVortojn(string peto) {
         var malinflekajŜtupoj = Sintaksanalizilo2.tuteMalinflekti(peto);
         string? malinflektitaVorto = null;
         Vorttipo.Vorttipo? malinflektitaTipo = null;
         string? bazo = null;
         if (malinflekajŜtupoj.IsOk) {
            var lastaŜtupo = malinflekajŜtupoj.ResultValue.Last() as Sintaksanalizilo.MalinflektaŜtupo.Bazo; 
            malinflektitaVorto = lastaŜtupo?.BazaVorto;
            malinflektitaTipo = lastaŜtupo?.Item1;
            bazo = Sintaksanalizilo2.bazoDe(malinflektitaVorto);
            var malinflektitaRezulto = await _amazonDynamoDbClient.QueryAsync(new QueryRequest(TableName) {
               ProjectionExpression = "vorto",
               KeyConditionExpression = "vorto = :v",
               ExpressionAttributeValues = new Dictionary<string, AttributeValue> {
                  {":v", new AttributeValue(malinflektitaVorto)}
               }
            });
            malinflektitaVorto = malinflektitaRezulto.Count == 1 ? malinflektitaRezulto.Items.First()["vorto"].S : null;
         }

         if (bazo != null) {
            var bazaRezulto = await _amazonDynamoDbClient.QueryAsync(new QueryRequest(TableName) {
               IndexName = "bazo-indekso",
               ProjectionExpression = "vorto",
               KeyConditionExpression = "bazo = :b",
               ExpressionAttributeValues = new Dictionary<string, AttributeValue>() {
                  {":b", new AttributeValue(bazo)}
               }
            });

            if (bazaRezulto.Count == 1) {
               var bazaVorto = bazaRezulto.Items.First()["vorto"].S;
               bazo = Sintaksanalizilo2.ĉuMalplenigita(malinflektitaTipo, bazaVorto) ? bazaVorto : null;
            }
            else {
               bazo = null;
            }
         }

         var rezultoj = await _amazonDynamoDbClient.ScanAsync(new ScanRequest(TableName) {
            ProjectionExpression = "vorto, signifo",
            FilterExpression = "contains(vorto, :p) OR contains(signifo, :p)",
            ExpressionAttributeValues = new Dictionary<string, AttributeValue> {
               {":p", new AttributeValue(peto)}
            }
         });

         if (rezultoj.LastEvaluatedKey.Count > 0) {
            throw new NotSupportedException("Pli da vortoj restantaj");
         }

         return new VortoRezulto {
            MalinflektitaVorto = malinflektitaVorto == peto ? null : malinflektitaVorto,
            PlenigitaVorto = bazo == peto ? null : bazo,
            Rezultoj = rezultoj.Items.Select(r => new VortoRespondo {
               Vorto = r["vorto"].S,
               Signifo = r["signifo"].S
            })
         };
      }
   }
}