namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type NombrigeblaKlasoTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("kresku", NombrigeblaKlaso, Infinitivo)
        ("kreski", NombrigeblaKlaso, Difinito)
        ("kreskinsa", NombrigeblaKlaso, Havaĵo)
        ("kreskuwa", NombrigeblaKlaso, PredikativoEsti)
        ("kreskuga", NombrigeblaKlaso, AtributivoEstiMalantaŭ)
        ("kreskireg", NombrigeblaKlaso, Havado)
        ("kreskilas", NombrigeblaKlaso, Translativo)
        ("kreskirim", NombrigeblaKlaso, Ekzistado)
        ("kreskiva", NombrigeblaKlaso, SpecifaĜerundo)
        ("kreskivra", NombrigeblaKlaso, Ĝerundo) ]
      |> List.map (fun (vorto, tipo, inflekcio) -> kontroliInflekcion tipo inflekcio vorto)
      |> ignore