namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type NetransitivaVerboTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("dulavis", Progresivo)
        ("dulavise", Hipoteza)
        ("dulaviso", Perfekto)
        ("dulavisela", Intenco)
        ("dulavisora", Desiderativo)
        ("dulavisie", Optativo)
        ("dulavisea", Imperativo)
        ("dulavisetie", Argumento1)
        ("dulaviselis", Translativo)
        ("dulavismea", Ĝerundo)
        ("dulavisem", PartaUjo1) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliInflekcion NetransitivaVerbo inflekcio vorto)
      |> ignore