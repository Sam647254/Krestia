namespace KrestiaVortilo.Testo

open FSharpx.Collections
open KrestiaVortilo
open Microsoft.VisualStudio.TestTools.UnitTesting
open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Sintaksanalizilo2
open KrestiaVortilo.Malinflektado

module Testiloj =
   let malsukcesi (_, eraro) = Assert.Fail eraro

   let plenaModifanto modifanto =
      { Modifanto = modifantojDePredikataVerboj.[modifanto]
        Vorto = testaVorto modifanto }

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

   let kontroliKategorigadon (frazo: string) (verboj: string list) (argumentoj: string list) =
      frazo.Split(' ')
      |> List.ofArray
      |> List.map testaVorto
      |> tuteMalinflektiĈiujn
      |> Result.bind (fun malinflektitaVortoj ->
            malinflektitaVortoj
            |> Sintaksanalizilo2.kategorigi Sintaksanalizilo2.kreiSintaksanalizilon
            |> Result.bind (fun sintaksanalizilo ->
                  verboj
                  |> List.map testaVorto
                  |> tuteMalinflektiĈiujn
                  |> Result.bind (fun malinflektitaVerboj ->
                        argumentoj
                        |> List.map testaVorto
                        |> tuteMalinflektiĈiujn
                        |> Result.map (fun malinflektitaArgumentoj ->
                              let verboj =
                                 malinflektitaVerboj |> List.map plenaVerbo

                              let argumentoj =
                                 malinflektitaArgumentoj
                                 |> List.map (fun a -> Sintaksanalizilo2.Argumento(a, Set.empty))

                              Assert.AreEqual(Deque.ofList verboj, Deque.toSeq sintaksanalizilo.Verboj)
                              Assert.AreEqual(Deque.ofList argumentoj, sintaksanalizilo.Argumentoj)))))
      |> Result.mapError malsukcesi
      |> ignore

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

   let kontroliArgumentojn eniro pravajArgumentoj =
      legi eniro true
      |> Result.map (fun rezulto -> Assert.AreEqual(pravajArgumentoj, rezulto.Argumentoj |> Deque.toSeq |> List.ofSeq))
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
