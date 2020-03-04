namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo
open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Sintaksanalizilo
open Testiloj

[<TestClass>]
type KlasoTestoj() =

   [<TestMethod>]
   member _.Infinitivoj() =
      [ "tatreto"; "ilitu"; "lustaa"; "geluko"; "kresku"; "trupaa"; "gremu"; "kunaa"; "verimaa"; "salumu"; "molomo"; "posmu" ]
      |> List.map (fun klaso ->
            Malinflektado.ĉuVerboInfinitivo klaso
            |> Result.map (fun rezulto ->
                  if rezulto then Assert.Fail(sprintf "%s ne estas verbo" klaso)
                  else ()))
      |> ignore

      [ "tatreto"; "ilitu"; "lustaa"; "geluko"; "kresku"; "trupaa" ]
      |> List.map (kontroliInflekcion NombrigeblaKlaso Infinitivo)
      |> ignore

      [ "gremu"; "kunaa"; "verimaa"; "salumu"; "molomo"; "posmu" ]
      |> List.map (kontroliInflekcion NenombrigeblaKlaso Infinitivo)
      |> ignore

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
   member _.Havaĵo() =
      [ ("trupansa", NombrigeblaKlaso, Havaĵo)
        ("trupasinsa", NombrigeblaKlaso, UnuHavaĵo)
        ("trupavensa", NombrigeblaKlaso, PluraHavaĵo)
        ("kunansa", NenombrigeblaKlaso, Havaĵo) ]
      |> List.map (fun (vorto, pravaTipo, pravaInflekcio) -> kontroliInflekcion pravaTipo pravaInflekcio vorto)
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