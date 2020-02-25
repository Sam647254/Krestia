﻿namespace KrestiaVortilo.Testo

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