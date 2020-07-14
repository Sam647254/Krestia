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
        ("kreskisi", NombrigeblaKlaso, UnuNombro)
        ("kreskinsa", NombrigeblaKlaso, Havaĵo)
        ("kreskisinsa", NombrigeblaKlaso, UnuHavaĵo)
        ("kreskivensa", NombrigeblaKlaso, PluraHavaĵo)
        ("kreskive", NombrigeblaKlaso, PluraNombro)
        ("kreskuwa", NombrigeblaKlaso, PredikativoEsti)
        ("kreskuga", NombrigeblaKlaso, AtributivoEstiMalantaŭ)
        ("kreskiris", NombrigeblaKlaso, Havado)
        ("kreskilas", NombrigeblaKlaso, Translativo)
        ("kreskirim", NombrigeblaKlaso, Ekzistado)
        ("kreskisirim", NombrigeblaKlaso, UnuEkzistado)
        ("kreskiverim", NombrigeblaKlaso, PluraEkzistado)
        ("kreskiva", NombrigeblaKlaso, SpecifaĜerundo)
        ("kreskivra", NombrigeblaKlaso, Ĝerundo) ]
      |> List.map (fun (vorto, tipo, inflekcio) -> kontroliInflekcion tipo inflekcio vorto)
      |> ignore