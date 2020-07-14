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
        ("gremuga", AtributivoEstiMalantaŭ)
        ("gremireg", Havado)
        ("gremilas", Translativo)
        ("gremirim", Ekzistado)
        ("gremiva", SpecifaĜerundo)
        ("gremivra", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliInflekcion NenombrigeblaKlaso inflekcio vorto)
      |> ignore