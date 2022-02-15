module KrestiaParser.Tests.NounTests

open NUnit.Framework

open KrestiaParser.Decompose
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

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
type NounTests() =

   [<Test>]
   member this.``Can recognize base countable nouns``() =
      for noun in countableNouns do
         decompose noun
         |> Option.orElseWith (fun () -> fail $"Could not decompose %s{noun}")
         |> Option.iter (fun result -> Assert.IsEmpty(result.steps))
