namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type KlasoTestoj () =

   [<TestMethod>]
   member _.Infinitivoj () =
      [ "tatreto"
        "ilitu"
        "lustaa"
        "geluko"
        "kresku"
        "trupaa" ]
      |> List.map (fun vorto -> kontroliFormon vorto NombrigeblaKlaso Infinitivo )
      |> ignore

      [ "gremu"
        "kunaa"
        "verimaa"
        "salumu"
        "molomo"
        "posmu"]
      |> List.map (fun vorto -> kontroliFormon vorto NenombrigeblaKlaso Infinitivo)
      |> ignore

   [<TestMethod>]
   member _.Substantivoj () =
      [ ("kreski", NombrigeblaEco)
        ("tatrete", NombrigeblaKlaso)
        ("duta", NombrigeblaKlaso)
        ("luna", NenombrigeblaEco)
        ("risme", NenombrigeblaEco) ]
      |> List.map (fun (vorto, pravaTipo) -> kontroliFormon vorto pravaTipo NekonitaNombro)
      |> ignore

      [ ("trupa", NekonitaNombro)
        ("trupasi", UnuNombro) 
        ("trupave", PluraNombro) ]
      |> List.map (fun (vorto, pravaInflekcio) -> kontroliFormon vorto NombrigeblaKlaso pravaInflekcio)