namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open Testiloj

[<TestClass>]
type EraroTestoj() =
   [<TestMethod>]
   member _.Testo1 () =
      kontroliPozonDeEraro "hem betre" 0 4