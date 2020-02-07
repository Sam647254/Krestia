namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SilaboTestoj() =
   
   [<TestMethod>]
   member _.SilaboTesto() =
      Testiloj.kontroliSilabojn "temigro" [ "te"; "mi" ]