﻿namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting
open KrestiaVortilo.Traktilaro
open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Legiloj
open KrestiaVortilo.Strukturo

module Testiloj =
   let kontroliFormon (vorto: string) (pravaTipo: Vorttipo) (pravaInflekcio: Inflekcio) =
      kontroli vorto
      |> Option.map
         (fun (tipo, formo) ->
            Assert.AreEqual(pravaTipo, tipo)
            Assert.AreEqual(pravaInflekcio, formo))
      |> Option.defaultWith (fun () -> Assert.Fail(sprintf "%s estas nevalida" vorto))

   let kontroliInflekcioj (vorto: string) (pravaInflekcioj: Vortformo list) (pravaMalinflektitaVorto: string) =
      malinflekti vorto
      |> Option.map
            (fun (formoj, malinflektitaVorto) ->
               Assert.AreEqual(pravaInflekcioj, formoj)
               Assert.AreEqual(pravaMalinflektitaVorto, malinflektitaVorto))
      |> Option.defaultWith (fun () -> Assert.Fail(sprintf "ne povis malinflekti %s" vorto))

   let kontroliFrazon (prava: Frazo) (frazo: string) =
      [ "evilel" ]
      |> set
      |> legiFrazon (frazo.Split(' ') |> List.ofArray) None
      |> Result.map
         (fun (rezulto, restantaj, _) ->
            Assert.AreEqual(0, restantaj.Length)
            Assert.AreEqual(prava, rezulto))
      |> Result.mapError (fun rezulto -> Assert.Fail(rezulto))
      |> ignore