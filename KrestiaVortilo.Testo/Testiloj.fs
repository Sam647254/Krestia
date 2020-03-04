﻿namespace KrestiaVortilo.Testo

open FSharpx.Collections
open KrestiaVortilo
open Microsoft.VisualStudio.TestTools.UnitTesting
open KrestiaVortilo.Traktilaro
open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Strukturo
open KrestiaVortilo.Sintaksanalizilo2
open KrestiaVortilo.Malinflektado

module Testiloj =
   let kontroliFormon (vorto: string) (pravaTipo: Vorttipo) (pravaInflekcio: Inflekcio) =
      kontroli vorto
      |> Option.map (fun (tipo, formo) ->
            Assert.AreEqual(pravaTipo, tipo)
            Assert.AreEqual(pravaInflekcio, formo))
      |> Option.defaultWith (fun () -> Assert.Fail(sprintf "%s estas nevalida" vorto))

   let kontroliFrazon (prava: Frazo) (frazo: string) =
      [ "evilel" ]
      |> set
      |> Legiloj.legiFrazon (frazo.Split(' ') |> List.ofArray) None
      |> Result.map (fun (rezulto, restantaj, _) ->
            Assert.AreEqual(prava, rezulto)
            Assert.AreEqual(0, restantaj.Length))
      |> Result.mapError Assert.Fail
      |> ignore

   let kontroliInflekcion (pravaTipo: Vorttipo) (pravaInflekcio: Inflekcio) (vorto: string) =
      malinflekti vorto
      |> Result.map (fun malinflektaŜtupo ->
            match malinflektaŜtupo with
            | Sintaksanalizilo.Bazo(vorttipo, inflekcio, _) ->
               Assert.AreEqual(pravaTipo, vorttipo)
               Assert.AreEqual(pravaInflekcio, inflekcio)
            | Sintaksanalizilo.Nebazo(vorttipo, inflekcio, _) ->
               Assert.AreEqual(pravaTipo, vorttipo)
               Assert.AreEqual(pravaInflekcio, inflekcio))
      |> Result.mapError Assert.Fail
      |> ignore

   let kontroliĈiujnInfleckiojn (ŝtupoj: Sintaksanalizilo.MalinflektaŜtupo list) (vorto: string) =
      tuteMalinflekti vorto
      |> Result.map (fun malinflektaVorto -> Assert.AreEqual(ŝtupoj, malinflektaVorto.InflekcioŜtupoj))
      |> Result.mapError Assert.Fail
      |> ignore

   let kontroliNevalidanVorton (vorto: string) =
      malinflekti vorto
      |> Result.map
            (fun malinflektaŜtupo ->
            Assert.Fail(sprintf "%s estas nevalida vorto, sed malinflektiĝis: %A" vorto malinflektaŜtupo))
      |> ignore

   let kontroliSilabojn (vorto: string, prava: string list) =
      dividi vorto false
      |> Result.map (fun rezulto -> Assert.AreEqual(rezulto, prava))
      |> Result.mapError Assert.Fail
      |> ignore

   let kontroliĈuPredikata (vorto: string) =
      tuteMalinflekti vorto
      |> Result.map ĉuPredikataVorto
      |> Result.mapError Assert.Fail

   let kontroliĈuArgumenta (vorto: string) =
      tuteMalinflekti vorto
      |> Result.map ĉuArgumentaVorto
      |> Result.mapError Assert.Fail

   let kontroliKategorigadon (frazo: string) (verboj: string list) (argumentoj: string list) =
      frazo.Split(' ')
      |> List.ofArray
      |> tuteMalinflektiĈiujn
      |> Result.bind (fun malinflektitaVortoj ->
            malinflektitaVortoj
            |> Sintaksanalizilo2.kategorigi Sintaksanalizilo2.kreiSintaksanalizilon
            |> Result.bind (fun sintaksanalizilo ->
                  verboj
                  |> tuteMalinflektiĈiujn
                  |> Result.bind (fun malinflektitaVerboj ->
                        argumentoj
                        |> tuteMalinflektiĈiujn
                        |> Result.map (fun malinflektitaArgumentoj ->
                              let verboj = malinflektitaVerboj |> List.map Sintaksanalizilo2.Verbo
                              let argumentoj = malinflektitaArgumentoj |> List.map Sintaksanalizilo2.Argumento
                              Assert.AreEqual
                                 ({ Verboj = Deque.ofList (verboj)
                                    Argumentoj = Deque.ofList (argumentoj) }, sintaksanalizilo)))))
      |> Result.mapError Assert.Fail
      |> ignore