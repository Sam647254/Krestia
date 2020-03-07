namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open Testiloj

[<TestClass>]
type Sintaksanalizilo() =

   [<TestMethod>]
   member _.``hem meratre gremi``() =
      // meratre(hem, gremi)
      kontroliKategorigadon "hem meratre gremi" [ "meratre" ] [ "hem"; "gremi" ]