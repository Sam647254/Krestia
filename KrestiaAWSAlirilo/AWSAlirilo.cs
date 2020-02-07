using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Amazon;
using Amazon.DynamoDBv2;
using Amazon.DynamoDBv2.Model;
using KrestiaVortaro;
using KrestiaVortilo;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using MoreLinq.Extensions;

namespace KrestiaAWSAlirilo {
   public class AwsAlirilo {
      private const string TableName = "Krestia-vortaro-2";

      private readonly AmazonDynamoDBClient _amazonDynamoDbClient = new AmazonDynamoDBClient(RegionEndpoint.USWest2);

      public async Task<IEnumerable<VortoRespondo>> AlportiĈiujnVortojn(bool baza = false) {
         var rezulto = await _amazonDynamoDbClient.ScanAsync(new ScanRequest {
            TableName = TableName,
            ProjectionExpression = baza ? "vorto, signifo" : "vorto, bazo, signifo, gloso, radikoj, kategorio, noto"
         });
         if (rezulto.LastEvaluatedKey.Count > 0) {
            throw new NotSupportedException("Pli da rezultoj restantaj");
         }

         return rezulto.Items.Select(vorto => new VortoRespondo(vorto["vorto"].S) {
            Vorto = vorto["vorto"].S,
            Bazo = vorto["bazo"].S,
            Signifo = vorto["signifo"].S,
            Gloso = vorto["gloso"].S,
            Kategorioj = vorto.GetValueOrDefault("kategorio")?.SS,
            Noto = vorto.GetValueOrDefault("noto")?.S,
            Radikoj = vorto.GetValueOrDefault("radikoj")?.SS
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
         var silaboj = Malinflektado.dividiKunFinaĵo(vorto);

         if (silaboj.IsError) {
            throw new Exception(silaboj.ErrorValue);
         }

         var vortoObjecto = respondo.Item;
         var inflekcioj = Malinflektado.ĉiujInflekciojDe(vorto);
         return !respondo.IsItemSet
            ? null
            : new VortoRespondo(vortoObjecto["vorto"].S) {
               Kategorioj = vortoObjecto.GetValueOrDefault("kategorioj")?.SS,
               Noto = vortoObjecto.GetValueOrDefault("noto")?.S,
               Radikoj = vortoObjecto.GetValueOrDefault("radikoj")?.SS,
               Signifo = vortoObjecto.GetValueOrDefault("signifo")?.S,
               Vorttipo = vorttipo,
               Silaboj = silaboj.ResultValue,
               InflektitajFormoj = FSharpOption<FSharpMap<Vorttipo.Inflekcio, string>>.get_IsSome(inflekcioj)
                  ? inflekcioj.Value.Select(p => (p.Key.ToString(), p.Value))
                     .ToDictionary()
                  : null
            };
      }

      public async Task AldoniVortojn(string eniro) {
         var vortaro = (await AlportiĈiujnVortojn()).Select(r => r.Vorto).ToImmutableHashSet();
         var vortoj = (await File.ReadAllLinesAsync(eniro)).Select(vico => {
            var partoj = vico.Split('|');
            if (partoj.Length != 6) {
               throw new ArgumentException($"{vico} estas nevalida");
            }

            var radikoj = partoj[4].Length > 0 ? partoj[3].Split(',').ToList() : null;
            var eraro = ĈuValidaVortaraVorto(vortaro, partoj[0], radikoj ?? new List<string>());
            if (eraro != null) {
               throw new ArgumentException(eraro);
            }

            return new VortoRespondo(partoj[0]) {
               Signifo = partoj[1],
               Gloso = partoj[2],
               Kategorioj = partoj[3].Length > 0 ? partoj[2].Split(',').ToList() : null,
               Radikoj = radikoj,
               Noto = partoj[5].Length > 0 ? partoj[4] : null
            };
         });
         await Task.WhenAll(vortoj.Select(vorto => {
            var peto = new Dictionary<string, AttributeValue> {
               {"vorto", new AttributeValue(vorto.Vorto)}, {
                  "bazo",
                  new AttributeValue(Malinflektado.bazoDe(vorto.Vorto))
               },
               {"gloso", new AttributeValue(vorto.Gloso)},
               {"signifo", new AttributeValue(vorto.Signifo)}
            };
            if (vorto.Kategorioj?.Count > 0) {
               peto["kategorioj"] = new AttributeValue(vorto.Kategorioj);
            }

            if (vorto.Radikoj?.Count > 0) {
               peto["radikoj"] = new AttributeValue(vorto.Radikoj);
            }

            if (vorto.Noto?.Length > 0) {
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

      private async Task<VortoRezulto> GlosaRezulto(
         IReadOnlyCollection<FSharpResult<Malinflektado.MalinflektitaVorto, string>> vortoj) {
         var bazoj = vortoj.Select(v => v.IsOk ? Malinflektado.bazoDe(v.ResultValue.BazaVorto) : "???");
         var rezultoj = await Task.WhenAll(bazoj.Select(b => _amazonDynamoDbClient.QueryAsync(
            new QueryRequest(TableName) {
               KeyConditionExpression = "bazo = :b",
               IndexName = "bazo-indekso",
               ProjectionExpression = "gloso, vorto",
               ExpressionAttributeValues = new Dictionary<string, AttributeValue>() {
                  {":b", new AttributeValue(b)}
               }
            })));

         if (rezultoj.All(r => r.Count == 0)) {
            throw new InvalidOperationException("Mankas vortojn");
         }

         return new VortoRezulto {
            GlosajVortoj = rezultoj.Select(r => r.Items.Count == 1 ? r.Items.First()["gloso"].S : "(not found)"),
            GlosajŜtupoj = vortoj.Select(v => v.IsOk
               ? v.ResultValue.InflekcioŜtupoj.Where(ŝ => ŝ.IsNebazo)
                  .Select(ŝ => ((Sintaksanalizilo.MalinflektaŜtupo.Nebazo) ŝ).Item2.ToString())
               : new List<string>()),
            BazajVortoj = rezultoj.Select(r => r.Items.Count == 1 ? r.Items.First()["vorto"].S : "")
         };
      }

      public async Task<IEnumerable<VortoRespondo>> AlportiVortojn(IEnumerable<string> vortoj) {
         var respondo = await _amazonDynamoDbClient.BatchGetItemAsync(new Dictionary<string, KeysAndAttributes>() {
            {
               TableName, new KeysAndAttributes {
                  AttributesToGet = new List<string> {"gloso", "vorto"},
                  Keys = vortoj.Select(v => new Dictionary<string, AttributeValue> {{"vorto", new AttributeValue(v)}})
                     .ToList()
               }
            }
         });

         if (respondo.UnprocessedKeys.Count > 0 && respondo.UnprocessedKeys[TableName].Keys.Count > 0) {
            throw new NotImplementedException("Tro da vortoj en la peto");
         }

         return respondo.Responses[TableName].Select(r => new VortoRespondo(r["vorto"].S) {
            Gloso = r["gloso"].S
         });
      }

      private static int Rilateco(VortoRespondo vortoRespondo, string peto) {
         if (peto == vortoRespondo.Vorto) {
            return 0;
         }

         if (vortoRespondo.Vorto.StartsWith(peto)) {
            return 1;
         }

         if (vortoRespondo.Signifo == peto) {
            return 2;
         }

         if (vortoRespondo.Signifo?.StartsWith(peto) == true) {
            return 3;
         }

         if (Regex.IsMatch(vortoRespondo.Signifo ?? "", $"\\b{peto}\\b", RegexOptions.IgnoreCase)) {
            return 4;
         }

         return int.MaxValue;
      }

      public static string? ĈuValidaVortaraVorto(ISet<string> ekzistantajVortoj, string novaVorto,
         IList<string> radikoj) {
         var malinflektitaVorto = Malinflektado.malinflekti(novaVorto);
         var ĉuHavasValidajnRadikojn = radikoj.All(ekzistantajVortoj.Contains);
         var ĉuValidaVorto = Malinflektado.dividi(novaVorto, inkluziFinaĵon: true);
         var malplenigitajVerboj = Malinflektado.malplenigitajFormojDe(novaVorto);
         var ĉuValidajMalplenigitajVerboj = malplenigitajVerboj.IsError || malplenigitajVerboj.ResultValue.All(m => {
            var ŝtupoj = Malinflektado.malinflekti(m);
            return ŝtupoj.IsOk && ŝtupoj.ResultValue.IsBazo;
         });

         if (!(malinflektitaVorto.IsOk && malinflektitaVorto.ResultValue.IsBazo)) {
            return $"{novaVorto} ne estas baza vorto";
         }

         if (ĉuValidaVorto.IsError) {
            return $"{novaVorto} ne havas validajn silabojn";
         }

         if (!ĉuHavasValidajnRadikojn) {
            return $"{novaVorto} ne havas validajn radikojn ({string.Join(',', radikoj)})";
         }

         if (!ĉuValidajMalplenigitajVerboj) {
            return $"{novaVorto} ne havas validajn malplenigitajn formojn";
         }

         return null;
      }
   }
}