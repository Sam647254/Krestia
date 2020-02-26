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
        ("buvitotrie", AtributativoEstiMalantaŭ)
        ("buvitotela", Estonteco)
        ("buvitotora", NominativoVolo)
        ("buvitotore", AkuzativoVolo)
        ("buvitotri", Imperativo)
        ("buvitotoniaa", Patiento)
        ("buvitotetio", Aganto)
        ("buvitotelit", Translativo)
        ("buvitotema", Ĝerundo)
        ("buvitotig", PartaNominativo)
        ("buvitotes", PartaAkuzativo)
        ("buvitotos", Pasivigo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto TransitivaVerbo2 inflekcio)
      |> ignore