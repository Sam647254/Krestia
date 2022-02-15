module KrestiaParser.Tests.NounTests

open NUnit.Framework

open KrestiaParser.Decompose
open KrestiaParser.WordType
open TestUtils

let private countableNouns =
   [ "vilipi"
     "brepe"
     "moropa"
     "kluti"
     "tote"
     "revita"
     "meki"
     "grike"
     "lurika" ]

let private uncountableNouns =
   [ "gremi"
     "rinome"
     "luvema"
     "lini"
     "kresne"
     "duna" ]

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
type NounTests() =

   [<Test>]
   member this.``Can recognize base countable nouns``() =
      this.CheckBaseWord CountableNoun countableNouns

   [<Test>]
   member this.``Can recognize base uncountable nouns``() =
      this.CheckBaseWord UncountableNoun uncountableNouns

   member private _.CheckBaseWord expectedType words =
      for word in words do
         decompose word
         |> Option.orElseWith (fun () -> fail $"Could not decompose %s{word}")
         |> Option.iter
               (fun result ->
                  Assert.IsEmpty(result.steps, $"%s{word} should not have any inflection steps")
                  Assert.AreEqual(expectedType, result.baseType, $"%s{word} should be of type %O{expectedType}")
                  Assert.AreEqual(word, result.baseWord, $"%s{word} should not have additional suffixes"))
