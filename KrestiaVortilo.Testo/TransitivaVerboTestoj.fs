namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type TransitivaVerboTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("buvitot", Progresivo)
        ("buvitotre", Hipoteza)
        ("buvitotro", Perfekto)
        ("buvitotrie", Optativo)
        ("buvitotela", Intenco)
        ("buvitotora", Desiderativo)
        ("buvitotore", Ujo2Volo)
        ("buvitotri", Imperativo)
        ("buvitotoniaa", Argumento2)
        ("buvitotetio", Argumento1)
        ("buvitotelit", Translativo)
        ("buvitotea", Ĝerundo)
        ("buvitotig", PartaUjo1)
        ("buvitotes", PartaUjo2) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliInflekcion TransitivaVerbo inflekcio vorto)
      |> ignore