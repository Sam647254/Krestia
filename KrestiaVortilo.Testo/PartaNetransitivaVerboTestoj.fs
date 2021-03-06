﻿namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type PartaNetransitivaVerboTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("omon", Progresivo)
        ("omonia", Hipoteza)
        ("omonio", Perfekto)
        ("omonela", Intenco)
        ("omoneri", Desiderativo)
        ("omonea", Ĝerundo)
        ("omonom", PartaUjo3) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliInflekcion NedirektaNetransitivaVerbo inflekcio vorto)
      |> ignore