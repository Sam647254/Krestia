namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type NenombrigeblaKlasoTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("gremu", Infinitivo)
        ("gremi", Difinito)
        ("gremuwa", PredikativoEsti)
        ("gremuga", AtributativoEstiMalantaŭ)
        ("gremura", PredikativoHavi)
        ("gremure", AtributativoHavi)
        ("gremilas", Translativo)
        ("gremirim", Ekzistado)
        ("gremiva", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto NenombrigeblaKlaso inflekcio)
      |> ignore