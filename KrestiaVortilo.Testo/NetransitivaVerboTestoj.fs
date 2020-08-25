namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type NetransitivaVerboTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("dulavis", Infinitivo)
        ("dulavise", Progresivo)
        ("dulaviso", Perfekto)
        ("dulavisela", Intenco)
        ("dulavisora", Desiderativo)
        ("dulavisie", AtributivoEstiMalantaŭ)
        ("dulavisea", Imperativo)
        ("dulavisetio", Argumento1)
        ("dulaviselis", Translativo)
        ("dulavismea", Ĝerundo)
        ("dulavisem", PartaUjo1) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliInflekcion NetransitivaVerbo inflekcio vorto)
      |> ignore