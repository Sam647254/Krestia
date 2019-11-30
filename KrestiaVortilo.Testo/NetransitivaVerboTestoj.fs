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
        ("dulavisie", AtributativoEsti)
        ("dulavisi", Imperativo)
        ("dulavisetio", Aganto)
        ("dulaviselis", Translativo)
        ("dulavisema", Ĝerundo)
        ("dulavisem", PartaNominativo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto TransitivaVerbo inflekcio)
      |> ignore