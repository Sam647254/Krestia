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

let private countablePIs =
    [ "vilipu"
      "brepo"
      "moropaa"
      "klutu"
      "toto"
      "revitaa"
      "meku"
      "griko"
      "lurikaa" ]

let private uncountablePIs =
    [ "gremu"
      "rinomo"
      "luvemaa"
      "linu"
      "kresno"
      "dunaa" ]

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
type NounTests() =

    [<Test>]
    member this.``Can recognize base countable nouns``() =
        this.CheckBaseWord CountableNoun countableNouns

    [<Test>]
    member this.``Can recognize base uncountable nouns``() =
        this.CheckBaseWord UncountableNoun uncountableNouns

    [<Test>]
    member this.``Can recognize predicative identity (countable)``() =
        this.CheckSingleInflection CountableNoun PredicativeIdentity countablePIs

    [<Test>]
    member this.``Can recognize predicative identity (uncountable)``() =
        this.CheckSingleInflection UncountableNoun PredicativeIdentity uncountablePIs

    [<Test>]
    member this.``Can recognize possession (countable)``() =
        this.CheckSingleSuffix CountableNoun Possession countableNouns "nsa"
    
    [<Test>]
    member this.``Can recognize possession (uncountable)``() =
        this.CheckSingleSuffix UncountableNoun Possession uncountableNouns "nsa"
    
    [<Test>]
    member this.``Can recognize AI (countable)``() =
        this.CheckSingleSuffix CountableNoun AttributiveIdentityPostfix countableNouns "ga"
        this.CheckSingleSuffix CountableNoun AttributiveIdentityPrefix countableNouns "va"
    
    [<Test>]
    member this.``Can recognize AI (uncountable)``() =
        this.CheckSingleSuffix UncountableNoun AttributiveIdentityPostfix uncountableNouns "ga"
        this.CheckSingleSuffix UncountableNoun AttributiveIdentityPrefix uncountableNouns "va"

    member private _.CheckBaseWord expectedType words =
        for word in words do
            optionTest $"Could not decompose %s{word}" {
                let! result = decomposeWord word
                Assert.IsEmpty(result.steps, $"%s{word} should not have any inflection steps")
                Assert.AreEqual(expectedType, result.baseType, $"%s{word} should be of type %O{expectedType}")
                Assert.AreEqual(word, result.baseWord, $"%s{word} should not have additional suffixes")
            }

    member private _.CheckSingleInflection expectedType (expectedInflection: Inflection) words =
        for word in words do
            optionTest $"Could not decompose %s{word}" {
                let! result = decomposeWord word
                Assert.AreEqual(expectedType, result.baseType, $"%s{word} should be of type %O{expectedType}")
                Assert.AreEqual([ expectedInflection ], result.steps)
            }

    member private _.CheckSingleSuffix expectedType expectedInflection words suffix =
        for word in words do
            let suffixed = word + suffix

            optionTest $"Could not decompose %s{suffixed}" {
                let! result = decomposeWord suffixed
                Assert.AreEqual(expectedType, result.baseType, $"%s{word} should be of type %O{expectedType}")

                Assert.AreEqual(
                    [ expectedInflection ],
                    result.steps,
                    $"%s{word} should have inflection %O{expectedInflection}"
                )

                Assert.AreEqual(word, result.baseWord)
            }
