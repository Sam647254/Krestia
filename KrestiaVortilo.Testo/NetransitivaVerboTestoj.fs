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
        ("dulavisela", Estonteco)
        ("dulavisera", Ujo1Volo)
        ("dulavisie", AtributivoEstiMalantaŭ)
        ("dulavisea", Imperativo)
        ("dulavisetio", Aganto)
        ("dulaviselis", Translativo)
        ("dulavisema", Ĝerundo)
        ("dulavisem", PartaUjo1) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto NetransitivaVerbo inflekcio)
      |> ignore