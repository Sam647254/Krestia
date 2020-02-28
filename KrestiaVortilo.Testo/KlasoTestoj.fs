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
      [ ("kreski", NombrigeblaKlaso)
        ("tatrete", NombrigeblaKlaso)
        ("duta", NombrigeblaKlaso)
        ("luna", NenombrigeblaKlaso)
        ("risme", NenombrigeblaKlaso) ]
      |> List.map (fun (vorto, pravaTipo) -> kontroliFormon vorto pravaTipo Difinito)
      |> ignore

      [ ("trupa", Difinito)
        ("trupasi", UnuNombro) 
        ("trupave", PluraNombro) ]
      |> List.map (fun (vorto, pravaInflekcio) -> kontroliFormon vorto NombrigeblaKlaso pravaInflekcio)
      |> ignore

   [<TestMethod>]
   member _.Havaĵo () =
      [ ("trupansa", NombrigeblaKlaso, Havaĵo)
        ("trupasinsa", NombrigeblaKlaso, UnuHavaĵo)
        ("trupavensa", NombrigeblaKlaso, PluraHavaĵo)
        ("kunansa", NenombrigeblaKlaso, Havaĵo) ]
      |> List.map (fun (vorto, pravaTipo, pravaInflekcio) ->
            kontroliFormon vorto pravaTipo pravaInflekcio)
      |> ignore

   [<TestMethod>]
   member _.Estado () =
      [ ("verikowa", PredikativoEsti)
        ("voritoga", AtributativoEstiMalantaŭ)
        ("voritova", AtributativoEstiAntaŭ) ]
      |> List.map (fun (vorto, pravaInflekcio) ->
            kontroliFormon vorto NombrigeblaKlaso pravaInflekcio)
      |> ignore

   [<TestMethod>]
   member _.Sola () =
      [ ("ilitivera", PluraSola)
        ("pospira", Sola) ]
      |> List.map (fun (vorto, pravaInflekcio) ->
            kontroliFormon vorto NombrigeblaKlaso pravaInflekcio)
      |> ignore

   [<TestMethod>]
   member _.Translativo () =
      [ ("ritmalas", NenombrigeblaKlaso)
        ("tretalas", NombrigeblaKlaso)
        ("tetalas", NombrigeblaKlaso)
        ("kentalas", NombrigeblaKlaso)
        ("kunalas", NenombrigeblaKlaso) ]
      |> List.map (fun (vorto, pravaTipo) ->
            kontroliFormon vorto pravaTipo Translativo)
      |> ignore

   [<TestMethod>]
   member _.Havado () =
      [ ("lekeris", Havado)
        ("lepasiris", UnuHavado)
        ("trupaveris", PluraHavado) ]
      |> List.map (fun (vorto, pravaInflekcio) ->
            kontroliFormon vorto NombrigeblaKlaso pravaInflekcio)
      |> ignore

   [<TestMethod>]
   member _.Ĝerundo () =
      [ ("verikevra", Ĝerundo)
        ("verikeva", SpecifaĜerundo) ]
      |> List.map (fun (vorto, pravaInflekcio) ->
            kontroliFormon vorto NombrigeblaKlaso pravaInflekcio)
      |> ignore