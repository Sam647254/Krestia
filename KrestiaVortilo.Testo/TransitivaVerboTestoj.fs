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
        ("buvitotri", Imperativo)
        ("buvitotonia", Argumento2)
        ("buvitotetie", Argumento1)
        ("buvitotelit", Translativo)
        ("buvitotea", Ĝerundo)
        ("buvitotig", PartaUjo1)
        ("buvitotes", PartaUjo2) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliInflekcion TransitivaVerbo inflekcio vorto)
      |> ignore