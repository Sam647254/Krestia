namespace KrestiaVortilo.Testo

open System
open System.Collections.Generic
open FSharpx.Collections
open KrestiaVortilo
open KrestiaVortilo.Imperativa
open Microsoft.VisualStudio.TestTools.UnitTesting
open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Sintaksanalizilo2
open KrestiaVortilo.Malinflektado

module Testiloj =
   let malsukcesi (_, eraro) = Assert.Fail eraro

   [<Obsolete>]
   let plenaModifanto modifanto = failwith "clean up"

   let kontroliInflekcion (pravaTipo: Vorttipo) (pravaInflekcio: Inflekcio) (vorto: string) =
      vorto
      |> testaVorto
      |> malinflekti
      |> Result.map (fun malinflektaŜtupo ->
            match malinflektaŜtupo with
            | Sintaksanalizilo.Bazo (vorttipo, inflekcio, _) ->
               Assert.AreEqual(pravaTipo, vorttipo)
               Assert.AreEqual(pravaInflekcio, inflekcio)
            | Sintaksanalizilo.Nebazo (vorttipo, inflekcio, _) ->
               Assert.AreEqual(pravaTipo, vorttipo)
               Assert.AreEqual(pravaInflekcio, inflekcio))
      |> Result.mapError malsukcesi
      |> ignore

   let aldoniFinaĵon (finaĵo: string) vorto = vorto + finaĵo

   let kontroliInflekciojn vortoj komunaFinaĵo pravaTipo pravaInflekcio =
      vortoj
      |> List.map (aldoniFinaĵon komunaFinaĵo)
      |> List.map (kontroliInflekcion pravaTipo pravaInflekcio)
      |> ignore

   let kontroliĈiujnInfleckiojn (ŝtupoj: Sintaksanalizilo.MalinflektaŜtupo list) (vorto: string) =
      vorto
      |> testaVorto
      |> tuteMalinflekti
      |> Result.map (fun malinflektaVorto -> Assert.AreEqual(ŝtupoj, malinflektaVorto.InflekcioŜtupoj))
      |> Result.mapError malsukcesi
      |> ignore

   let kontroliNevalidanVorton (vorto: string) =
      vorto
      |> testaVorto
      |> malinflekti
      |> Result.map (fun malinflektaŜtupo ->
            Assert.Fail(sprintf "%s estas nevalida vorto, sed malinflektiĝis: %A" vorto malinflektaŜtupo))
      |> ignore

   let kontroliNevalidanFrazon (eniro: string) =
      analizi eniro true
      |> Result.map (fun rezulto -> Assert.Fail(sprintf "%s estas nevalida frazo, sed legis %A" eniro rezulto))
      |> ignore

   let kontroliSilabojn (vorto: string, prava: string list) =
      dividi vorto false
      |> Result.map (fun rezulto -> Assert.AreEqual(rezulto, prava))
      |> Result.mapError Assert.Fail
      |> ignore

   let kontroliĈuPredikata (vorto: string) =
      vorto
      |> testaVorto
      |> tuteMalinflekti
      |> Result.map ĉuPredikataVorto
      |> Result.mapError malsukcesi

   let kontroliĈuArgumenta (vorto: string) =
      vorto
      |> testaVorto
      |> tuteMalinflekti
      |> Result.map ĉuArgumentaVorto
      |> Result.mapError malsukcesi

   let kontroliKategorigadon (frazo: string) (verboj: string list) (argumentoj: string list) = failwith "forigi"

   let kontroliMalplenigitajnFormojn (vorto: string) (pravajFormoj: string list) =
      malplenigitajFormojDe vorto
      |> Result.map (fun rezultoj -> Assert.AreEqual(rezultoj, pravajFormoj |> Set.ofList))
      |> Result.mapError Assert.Fail
      |> ignore

   let praveMalinflekti ĉeno =
      match ĉeno |> testaVorto |> tuteMalinflekti with
      | Ok (rezulto) -> rezulto
      | Error (_, eraro) ->
         Assert.Fail(eraro)
         failwith eraro

   let kontroliUnuFrazon eniro (prava: Predikato) =
      analizi eniro true
      |> Result.map (fun rezulto ->
            Assert.AreEqual(1, rezulto.Frazoj.Length)
            Assert.AreEqual(prava, rezulto.Frazoj.Item 0))
      |> Result.mapError malsukcesi
      |> ignore

   let kontroliPlurajnFrazojn eniro (pravaj: Predikato list) (restantaj: Argumento list) =
      analizi eniro true
      |> Result.map (fun rezulto ->
            Assert.AreEqual(pravaj, rezulto.Frazoj)
            Assert.AreEqual(restantaj, rezulto.RestantajVortoj))
      |> Result.mapError malsukcesi
      |> ignore

   let kontroliArgumentojn eniro (pravajArgumentoj: Argumento list) =
      prepariEniron eniro true
      |> Result.bind (fun vortoj ->
            let legilo = ImperitivaLegilo(Queue(vortoj))
            legilo.Legi()
            |> Result.map (fun rezulto ->
                  Assert.AreEqual(pravajArgumentoj.Length, rezulto.Argumentoj.Length)
                  pravajArgumentoj
                  |> Seq.zip rezulto.Argumentoj
                  |> Seq.forall (fun (aktuala, prava) ->
                        Assert.AreEqual(prava.Vorto.Kapo, aktuala.Vorto.Kapo)
                        Assert.IsTrue(prava.Vorto.Modifantoj.SetEquals(aktuala.Vorto.Modifantoj))
                        true)))
      |> Result.mapError malsukcesi
      |> ignore

   let kontroliFrazojn eniro (pravajFrazoj: Predikato list) =
      prepariEniron eniro true
      |> Result.bind (fun vortoj ->
            let legilo = vortoj |> Queue |> ImperitivaLegilo
            legilo.Legi()
            |> Result.map (fun rezulto -> Assert.AreEqual(pravajFrazoj, rezulto.Frazoj)))
      |> Result.mapError malsukcesi
      |> ignore

   let kontroliRestantajnVortojn eniro (pravajRestantaj: Argumento list) =
      analizi eniro true
      |> Result.map (fun rezulto -> Assert.AreEqual(pravajRestantaj, rezulto.RestantajVortoj))
      |> Result.mapError malsukcesi
      |> ignore

   let kontroliPozonDeEraro eniro vico pozo =
      analizi eniro false
      |> Result.map (fun _ -> Assert.Fail(sprintf "%s estas nevalida, sed estas sukcese legita" eniro))
      |> Result.mapError (fun (vorto, _) ->
            Assert.AreEqual(vico, vorto.Vico)
            Assert.AreEqual(pozo, vorto.Pozo))
      |> ignore
