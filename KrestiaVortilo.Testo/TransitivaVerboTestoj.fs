namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type TransitivaVerboTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("buvitok", Infinitivo)
        ("buvitokre", Progresivo)
        ("buvitokro", Perfekto)
        ("buvitokrie", AtributativoEsti)
        ("buvitokri", Imperativo)
        ("buvitokoniaa", Patiento)
        ("buvitoketio", Aganto)
        ("buvitokelit", Translativo)
        ("buvitokema", Ĝerundo)
        ("buvitokeg", PartaNominativo)
        ("buvitokes", PartaAkuzativo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto TransitivaVerbo inflekcio)
      |> ignore