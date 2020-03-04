namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type TransitivaVerboTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("buvitot", Infinitivo)
        ("buvitotre", Progresivo)
        ("buvitotro", Perfekto)
        ("buvitotrie", AtributivoEstiMalantaŭ)
        ("buvitotela", Estonteco)
        ("buvitotora", Ujo1Volo)
        ("buvitotore", Ujo2Volo)
        ("buvitotri", Imperativo)
        ("buvitotoniaa", Patiento)
        ("buvitotetio", Aganto)
        ("buvitotelit", Translativo)
        ("buvitotema", Ĝerundo)
        ("buvitotig", PartaUjo1)
        ("buvitotes", PartaUjo2)
        ("buvitotos", Pasivigo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto TransitivaVerbo inflekcio)
      |> ignore