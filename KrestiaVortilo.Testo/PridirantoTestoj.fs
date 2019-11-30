namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type PridirantoTestoj () =
   
   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("perid", Infinitivo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto Pridiranto inflekcio)
      |> ignore