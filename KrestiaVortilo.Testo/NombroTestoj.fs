namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting
open Testiloj

[<TestClass>]
type NombroTestoj() =
   [<TestMethod>]
   member _.NaturajNombroj() =
      kontroliKalkulon "sona" 7.0
      kontroliKalkulon "po mi vo nona" 1023.0
      kontroliKalkulon "te si li sona" 4567.0
      kontroliKalkulon "ke gi po mira" 8910.0
   
   [<TestMethod>]
   member _.Kalkulado() =
      kontroliKalkulon "sona tikal nona" 10.0
      kontroliKalkulon "sona senal nona" 4.0
      kontroliKalkulon "sona petal nona" 21.0
      kontroliKalkulon "sona visal nona" (7.0/3.0)

   [<TestMethod>]
   member _.Kalkulado2() =
      kontroliKalkulon "sona tikal nona tikal vora" 12.0
      kontroliKalkulon "sona tikal nona nil tikal vora" 12.0
      kontroliKalkulon "sona petal nona nil tikal nona" 24.0
      kontroliKalkulon "sona petal nona tikal nona" 42.0
      