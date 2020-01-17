namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type NenombrigeblaKlasoTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("gremu", Infinitivo)
        ("gremi", NekonitaNombro)
        ("gremuwa", PredikativoEsti)
        ("gremuga", AtributativoEsti)
        ("gremura", PredikativoHavi)
        ("gremure", AtributativoHavi)
        ("gremilas", Translativo)
        ("gremirim", Ekzistado)
        ("gremiva", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto NenombrigeblaKlaso inflekcio)
      |> ignore