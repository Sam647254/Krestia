﻿namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo
open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Sintaksanalizilo
open Testiloj

[<TestClass>]
type KlasoTestoj() =
   let difinitoj = [ "vilipi"; "brepe"; "moropa"; "kluti"; "tote"; "revita"; "meki"; "grike"; "lurika" ]
   let difinitoj2 = [ "gremi"; "rinome"; "luvema"; "lini"; "kresne"; "duna" ]
   let predikatoj = [ "vilipu"; "brepo"; "moropaa"; "klutu"; "toto"; "revitaa"; "meku"; "griko"; "lurikaa" ]
   let predikatoj2 = [ "gremu"; "rinomo"; "luvemaa"; "linu"; "kresno"; "dunaa" ]
   
   [<TestMethod>]
   member _.Difinito() =
      difinitoj
      |> List.map (kontroliInflekcion NombrigeblaKlaso Difinito)
      |> ignore
      
      difinitoj2
      |> List.map (kontroliInflekcion NenombrigeblaKlaso Difinito)
      |> ignore
   
   [<TestMethod>]
   member _.PredikativoEsti() =
      predikatoj
      |> List.map (kontroliInflekcion NombrigeblaKlaso PredikativoEsti)
      |> ignore
      
      predikatoj2
      |> List.map (kontroliInflekcion NenombrigeblaKlaso PredikativoEsti)
      |> ignore
      
   [<TestMethod>]
   member _.UnuNombro() =
      difinitoj
      |> List.map (aldoniFinaĵon "si")
      |> List.map (kontroliInflekcion NombrigeblaKlaso UnuNombro)
      |> ignore
      
   [<TestMethod>]
   member _.PluraNombro() =
      difinitoj
      |> List.map (aldoniFinaĵon "ve")
      |> List.map (kontroliInflekcion NombrigeblaKlaso PluraNombro)
      |> ignore
   
   [<TestMethod>]
   member _.Havaĵo() =
      kontroliInflekciojn difinitoj "nsa" NombrigeblaKlaso Havaĵo
      kontroliInflekciojn difinitoj "sinsa" NombrigeblaKlaso UnuHavaĵo
      kontroliInflekciojn difinitoj "vensa" NombrigeblaKlaso PluraHavaĵo

   [<TestMethod>]
   member _.Substantivoj() =
      [ ("kreski", NombrigeblaKlaso)
        ("tatrete", NombrigeblaKlaso)
        ("duta", NombrigeblaKlaso)
        ("luna", NenombrigeblaKlaso)
        ("risme", NenombrigeblaKlaso) ]
      |> List.map (fun (vorto, pravaTipo) -> kontroliInflekcion pravaTipo Difinito vorto)
      |> ignore

      [ ("trupa", Difinito)
        ("trupasi", UnuNombro)
        ("trupave", PluraNombro) ]
      |> List.map (fun (vorto, pravaInflekcio) -> kontroliInflekcion NombrigeblaKlaso pravaInflekcio vorto)
      |> ignore

   [<TestMethod>]
   member _.Estado() =
      [ ("verikowa", PredikativoEsti)
        ("voritoga", AtributivoEstiMalantaŭ)
        ("voritova", AtributivoEstiAntaŭ) ]
      |> List.map (fun (vorto, pravaInflekcio) -> kontroliInflekcion NombrigeblaKlaso pravaInflekcio vorto)
      |> ignore

   [<TestMethod>]
   member _.Sola() =
      [ ("ilitivera", PluraSola)
        ("pospira", Sola) ]
      |> List.map (fun (vorto, pravaInflekcio) -> kontroliInflekcion NombrigeblaKlaso pravaInflekcio vorto)
      |> ignore

   [<TestMethod>]
   member _.Translativo() =
      [ ("ritmalas", NenombrigeblaKlaso)
        ("tretalas", NombrigeblaKlaso)
        ("tetalas", NombrigeblaKlaso)
        ("kentalas", NombrigeblaKlaso)
        ("kunalas", NenombrigeblaKlaso) ]
      |> List.map (fun (vorto, pravaTipo) -> kontroliInflekcion pravaTipo Translativo vorto)
      |> ignore

   [<TestMethod>]
   member _.Havado() =
      [ ("lekereg", Havado)
        ("lepasireg", UnuHavado)
        ("trupavereg", PluraHavado) ]
      |> List.map (fun (vorto, pravaInflekcio) -> kontroliInflekcion NombrigeblaKlaso pravaInflekcio vorto)
      |> ignore

   [<TestMethod>]
   member _.Ĝerundo() =
      [ ("verikevra", Ĝerundo)
        ("verikeva", SpecifaĜerundo) ]
      |> List.map (fun (vorto, pravaInflekcio) -> kontroliInflekcion NombrigeblaKlaso pravaInflekcio vorto)
      |> ignore

   [<TestMethod>]
   member _.NevalidajVortoj() =
      [ "kunasi"; "kunasira"; "mo"; "ko"; "tu"; "ti"; "tira" ]
      |> List.map kontroliNevalidanVorton
      |> ignore

      kontroliInflekcion NetransitivaVerbo Infinitivo "kunaveris" |> ignore

   [<TestMethod>]
   member _.PlurajInflekcioj() =
      "kinarimela"
      |> kontroliĈiujnInfleckiojn
            [ Nebazo(MalplenaVerbo, Estonteco, "kinarim")
              Nebazo(NenombrigeblaKlaso, Ekzistado, "kinaa")
              Bazo(NenombrigeblaKlaso, Infinitivo, "kinaa") ]

      "kunalasmea"
      |> kontroliĈiujnInfleckiojn
            [ Nebazo(NetransitivaVerbo, Ĝerundo, "kunalas")
              Nebazo(NenombrigeblaKlaso, Translativo, "kunaa")
              Bazo(NenombrigeblaKlaso, Infinitivo, "kunaa") ]
      
      "nekeveregore"
      |> kontroliĈiujnInfleckiojn
            [ Nebazo(OblikaNetransitivaVerbo, Ujo2Volo, "nekevereg")
              Nebazo(NombrigeblaKlaso, PluraHavado, "neko")
              Bazo(NombrigeblaKlaso, Infinitivo, "neko") ]