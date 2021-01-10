namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting
open Testiloj

[<TestClass>]
type NombroTestoj() =
   [<TestMethod>]
   member _.NaturajNombroj() =
      kontroliNombron "sona" 7m
      kontroliNombron "poni miri vori nona" 1023m
      kontroliNombron "teri sini liri sona" 4567m
      kontroliNombron "keri gini pomira" 8910m
   
   [<TestMethod>]
   member _.Kalkulado() =
      kontroliKalkulon "sona tikal nona" 10m