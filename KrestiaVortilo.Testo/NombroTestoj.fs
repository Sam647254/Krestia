namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting
open Testiloj

[<TestClass>]
type NombroTestoj() =
   [<TestMethod>]
   member _.NaturajNombroj() =
      kontroliKalkulon "sona" 7m
      kontroliKalkulon "po mi vo nona" 1023m
      kontroliKalkulon "te si li sona" 4567m
      kontroliKalkulon "ke gi po mira" 8910m
   
   [<TestMethod>]
   member _.Kalkulado() =
      kontroliKalkulon "sona tikal nona" 10m
      kontroliKalkulon "sona senal nona" 4m
      kontroliKalkulon "sona petal nona" 21m
      kontroliKalkulon "sona visal nona" (7m/3m)

   [<TestMethod>]
   member _.Kalkulado2() =
      kontroliKalkulon "sona tikal nona tikal vora" 12m
      kontroliKalkulon "sona tikal nona nil tikal vora" 12m
      kontroliKalkulon "sona petal nona nil tikal nona" 24m
      kontroliKalkulon "sona petal nona tikal nona" 42m
      