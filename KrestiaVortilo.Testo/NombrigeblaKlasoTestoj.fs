namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type NombrigeblaKlasoTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("kresku", NombrigeblaKlaso, Infinitivo)
        ("kreski", NombrigeblaKlaso, NekonitaNombro)
        ("kreskisi", NombrigeblaKlaso, UnuNombro)
        ("kreskinsa", NombrigeblaKlaso, Havaĵo)
        ("kreskisinsa", NombrigeblaKlaso, Havaĵo)
        ("kreskivensa", NombrigeblaKlaso, Havaĵo)
        ("kreskive", NombrigeblaKlaso, PluraNombro)
        ("kreskuwa", NombrigeblaKlaso, PredikativoEsti)
        ("kreskuga", NombrigeblaKlaso, AtributativoEsti)
        ("kreskira", NombrigeblaKlaso, PredikativoHavi)
        ("kreskire", NombrigeblaKlaso, AtributativoHavi)
        ("kreskilas", NombrigeblaKlaso, Translativo)
        ("kreskirim", NombrigeblaKlaso, Ekzistado)
        ("kreskisirim", NombrigeblaKlaso, Ekzistado)
        ("kreskiverim", NombrigeblaKlaso, Ekzistado)
        ("kreskiva", NombrigeblaKlaso, Ĝerundo) ]
      |> List.map (fun (vorto, tipo, inflekcio) -> kontroliFormon vorto tipo inflekcio)
      |> ignore