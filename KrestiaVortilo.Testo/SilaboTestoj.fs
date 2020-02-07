namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SilaboTestoj() =

   [<TestMethod>]
   member _.SilaboTesto() =
      [ "temigro", [ "te"; "mi" ]
        "liveras", [ "li"; "ve"; "ra" ]
        "krestia", [ "kres"; "ti"; "a" ] ]
      |> List.map Testiloj.kontroliSilabojn
      |> ignore