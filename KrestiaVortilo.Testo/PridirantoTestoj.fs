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
        ("peridie", PluraNombro)
        ("peridia", Progresivo)
        ("perido", Perfekto)
        ("peridea", AtributativoEsti)
        ("peridis", Translativo)
        ("peridi", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto Pridiranto inflekcio)
      |> ignore