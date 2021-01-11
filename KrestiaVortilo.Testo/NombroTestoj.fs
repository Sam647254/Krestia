namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting
open Testiloj

[<TestClass>]
type NombroTestoj() =
   [<TestMethod>]
   member _.NaturajNombroj() =
      kontroliKalkulon "sona" 7m
      kontroliKalkulon "poni miri vori nona" 1023m
      kontroliKalkulon "teri sini liri sona" 4567m
      kontroliKalkulon "keri gini pomira" 8910m
   
   [<TestMethod>]
   member _.Kalkulado() =
      kontroliKalkulon "sona tikal nona" 10m