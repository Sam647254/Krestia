﻿namespace KrestiaVortilo.Testo

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
        ("dulavisera", NominativoVolo)
        ("dulaviseri", DativoVolo)
        ("dulavisie", AtributativoEsti)
        ("dulavisea", Imperativo)
        ("dulavisetio", Aganto)
        ("dulaviselis", Translativo)
        ("dulavisema", Ĝerundo)
        ("dulavisem", PartaNominativo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto NetransitivaVerbo inflekcio)
      |> ignore