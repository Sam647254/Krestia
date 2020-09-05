namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type PridirantoTestoj () =
   
   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("perid", Infinitivo)
        ("peride", Difinito)
        ("peridu", Havaĵo)
        ("peridia", PredikativoEsti)
        ("peridea", AtributivoEstiMalantaŭ)
        ("peridis", Translativo)
        ("peridi", Ĝerundo)
        ("peridet", Igo)
        ("peridod", Etigo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliInflekcion Pridiranto inflekcio vorto)
      |> ignore