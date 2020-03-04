namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open Testiloj

[<TestClass>]
type Sintaksanalizilo() =

   [<TestMethod>]
   member _.``hem meratre gremi``() =
      // meratre(hem, gremi)
      kontroliKategorigadon "hem meratre gremi" [ "meratre" ] [ "hem"; "gremi" ]


//      let prava =
//         Predikato2(Verbo meratre, Argumento hem, Argumento gremi)
//      Assert.AreEqual(prava, legiFrazon "hem meratre gremi")
