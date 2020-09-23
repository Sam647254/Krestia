namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting
open KrestiaVortilo.Sintaksanalizilo2
open Testiloj

[<TestClass>]
type GitHubTestoj() =
   
   [<TestMethod>]
   // https://github.com/Sam647254/Krestia/issues/1
   member _.``Parser can't parse "liverasea linetiga"``() =
      let [ liverasea; linetiga ] = [ "liverasea"; "linetiga" ] |> List.map praveMalinflekti
      kontroliFrazojn
         "liverasea linetiga"
         [ { Kapo = verbo liverasea [ pridiranto linetiga ]
             Argumentoj = [] } ]