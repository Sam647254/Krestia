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
        ("kreskive", NombrigeblaKlaso, PluraNombro)
        ("kreskuwa", NombrigeblaKlaso, PredikativoEsti)
        ("kreskuga", NombrigeblaKlaso, AtributativoEsti)
        ("kreskura", NombrigeblaKlaso, PredikativoHavi)
        ("kreskure", NombrigeblaKlaso, AtributativoHavi)
        ("kreskilas", NombrigeblaKlaso, Translativo)
        ("kreskiva", NombrigeblaKlaso, Ĝerundo) ]
      |> List.map (fun (vorto, tipo, inflekcio) -> kontroliFormon vorto tipo inflekcio)
      |> ignore

   [<TestMethod>]
   member _.MalinflektiTesto1 () =
      [ ("kresku", [ (NombrigeblaKlaso, Infinitivo) ], "kresku")
        ("kreski", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, NekonitaNombro) ], "kresku")
        ("kreskisi", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, UnuNombro) ], "kresku") ]
      |> List.map
         (fun (inflektitaVorto, pravaListo, pravaMalinflektitaVorto) ->
            kontroliInflekcioj inflektitaVorto pravaListo pravaMalinflektitaVorto)
      |> ignore

   [<TestMethod>]
   member _.MalinflektiTesto2 () =
      [ ("kreskuwa", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, PredikativoEsti) ], "kresku")
        ("kreskuga", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, AtributativoEsti) ], "kresku")
        ("kreskura", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, PredikativoHavi) ], "kresku") ]
      |> List.map
         (fun (inflektitaVorto, pravaListo, pravaMalinflektitaVorto) ->
            kontroliInflekcioj inflektitaVorto pravaListo pravaMalinflektitaVorto)
      |> ignore

   [<TestMethod>]
   member _.MalinflektiTesto3 () =
      [ ("kreskure", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, AtributativoHavi) ], "kresku")
        ("kreskilas", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, Translativo) ], "kresku")
        ("kreskiva", [ (NombrigeblaKlaso, Infinitivo); (NombrigeblaKlaso, Ĝerundo) ], "kresku") ]
      |> List.map
         (fun (inflektitaVorto, pravaListo, pravaMalinflektitaVorto) ->
            kontroliInflekcioj inflektitaVorto pravaListo pravaMalinflektitaVorto)
      |> ignore