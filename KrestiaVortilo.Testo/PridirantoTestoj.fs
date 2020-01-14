namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type PridirantoTestoj () =
   
   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("perid", Infinitivo)
        ("peride", NekonitaNombro)
        ("perida", UnuNombro)
        ("peridu", Havaĵo)
        ("peridie", PluraNombro)
        ("peridia", PredikativoEsti)
        ("perido", Perfekto)
        ("peridea", AtributativoEsti)
        ("peridis", Translativo)
        ("peridi", Ĝerundo)
        ("peridet", Igo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto Pridiranto inflekcio)
      |> ignore