﻿namespace KrestiaVortilo.Testo

open KrestiaVortilo.Imperativa
open KrestiaVortilo.Sintaksanalizilo2
open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Sintaksanalizilo
open Testiloj

[<TestClass>]
type KlasoTestoj() =
   let difinitoj =
      [ "vilipi"
        "brepe"
        "moropa"
        "kluti"
        "tote"
        "revita"
        "meki"
        "grike"
        "lurika" ]

   let difinitoj2 =
      [ "gremi"
        "rinome"
        "luvema"
        "lini"
        "kresne"
        "duna" ]

   let predikatoj =
      [ "vilipu"
        "brepo"
        "moropaa"
        "klutu"
        "toto"
        "revitaa"
        "meku"
        "griko"
        "lurikaa" ]

   let predikatoj2 =
      [ "gremu"
        "rinomo"
        "luvemaa"
        "linu"
        "kresno"
        "dunaa" ]

   [<TestMethod>]
   member _.Difinito() =
      kontroliInflekciojn difinitoj2 "" NenombrigeblaKlaso Difinito
      kontroliInflekciojn difinitoj2 "" NenombrigeblaKlaso Difinito

   [<TestMethod>]
   member _.PredikativoEsti() =
      kontroliInflekciojn predikatoj "" NombrigeblaKlaso PredikativoEsti
      kontroliInflekciojn predikatoj2 "" NenombrigeblaKlaso PredikativoEsti

   [<TestMethod>]
   member _.Havaĵo() =
      kontroliInflekciojn difinitoj "nsa" NombrigeblaKlaso Havaĵo

      kontroliInflekciojn difinitoj2 "nsa" NenombrigeblaKlaso Havaĵo

   [<TestMethod>]
   member _.Fokuso() =
      kontroliInflekciojn difinitoj "le" NombrigeblaKlaso Fokuso

      kontroliInflekciojn difinitoj2 "le" NenombrigeblaKlaso Fokuso

   [<TestMethod>]
   member _.AtributivoEstiMalantaŭ() =
      kontroliInflekciojn difinitoj "ga" NombrigeblaKlaso AtributivoEstiMalantaŭ

      kontroliInflekciojn difinitoj2 "ga" NenombrigeblaKlaso AtributivoEstiMalantaŭ

   [<TestMethod>]
   member _.AtributivoEstiAntaŭ() =
      kontroliInflekciojn difinitoj "va" NombrigeblaKlaso AtributivoEstiAntaŭ

      kontroliInflekciojn difinitoj2 "va" NenombrigeblaKlaso AtributivoEstiAntaŭ

   [<TestMethod>]
   member _.AtributivoEstiLegado() =
      let _ =
         let vortoj =
            List.map praveMalinflekti [ "imilta"; "kunataga" ]

         match vortoj with
         | [ imilta; kunataga ] ->
            kontroliArgumentojn
               "imilta kunataga"
               [ argumento imilta (List.singleton (Modifanto.Pridiranto <| argumento kunataga [])) ]
         | _ -> Assert.Fail()

      let _ =
         let vortoj =
            List.map praveMalinflekti [ "kunatava"; "imilta" ]

         match vortoj with
         | [ kunatava; imilta ] ->
            kontroliArgumentojn
               "kunatava imilta"
               [ argumento imilta (List.singleton (Modifanto.Pridiranto <| argumento kunatava [])) ]
         | _ -> Assert.Fail()

      let _ =
         let vortoj =
            List.map praveMalinflekti [ "kunatava"; "imilta"; "rimaga" ]

         match vortoj with
         | [ kunatava; imilta; rimaga ] ->
            kontroliArgumentojn
               "kunatava imilta rimaga"
               [ argumento
                  imilta
                     [ Modifanto.Pridiranto <| argumento kunatava []
                       Modifanto.Pridiranto <| argumento rimaga [] ] ]
         | _ -> Assert.Fail()

      let _ =
         let vortoj =
            List.map praveMalinflekti [ "kunatava"; "rimava"; "imilta" ]

         match vortoj with
         | [ kunatava; rimava; imilta ] ->
            kontroliArgumentojn
               "kunatava rimava imilta"
               [ argumento
                  imilta
                    [ Modifanto.Pridiranto <| argumento kunatava []
                      Modifanto.Pridiranto <| argumento rimava [] ] ]
         | _ -> Assert.Fail()

      ()

   [<TestMethod>]
   member _.Sola() =
      kontroliInflekciojn difinitoj "ra" NombrigeblaKlaso Sola

      kontroliInflekciojn difinitoj2 "ra" NenombrigeblaKlaso Sola

      let _ =
         let imiltara = praveMalinflekti "imiltara"
         kontroliFrazojn
            "imiltara"
            [ { Kapo = verbo imiltara []
                Argumentoj = [] } ]

      let _ =
         match List.map praveMalinflekti [ "imiltara"; "kunataga" ] with
         | [ imiltara; kunataga ] ->
            kontroliFrazojn
               "imiltara kunataga"
               [ { Kapo = verbo imiltara [ Modifanto.Pridiranto <| argumento kunataga [] ]
                   Argumentoj = [] } ]
         | _ -> Assert.Fail()

      let _ =
         match List.map praveMalinflekti [ "kunatava"; "imiltara" ] with
         | [ kunatava; imiltara ] ->
            kontroliFrazojn
               "kunatava imiltara"
               [ { Kapo = verbo imiltara [ Modifanto.Pridiranto <| argumento kunatava [] ]
                   Argumentoj = [] } ]
         | _ -> Assert.Fail()

      ()


   [<TestMethod>]
   member _.Havado() =
      [ "res", Havado
        "rem", Havado ]
      |> List.map (fun (finaĵo, inflekcio) -> kontroliInflekciojn difinitoj finaĵo NombrigeblaKlaso inflekcio)
      |> ignore

      kontroliInflekciojn difinitoj2 "res" NenombrigeblaKlaso Havado
      kontroliInflekciojn difinitoj2 "rem" NenombrigeblaKlaso Havado
      ()

   [<TestMethod>]
   member _.Ekzistado() =
      [ "rim", Ekzistado ]
      |> List.map (fun (finaĵo, inflekcio) -> kontroliInflekciojn difinitoj finaĵo NombrigeblaKlaso inflekcio)
      |> ignore

      kontroliInflekciojn difinitoj2 "rim" NenombrigeblaKlaso Ekzistado

   [<TestMethod>]
   member _.Translativo() =
      kontroliInflekciojn predikatoj "las" NombrigeblaKlaso Translativo
      kontroliInflekciojn predikatoj2 "las" NenombrigeblaKlaso Translativo

   [<TestMethod>]
   member _.Ĝerundo() =
      kontroliInflekciojn predikatoj "vra" NombrigeblaKlaso Ĝerundo
      kontroliInflekciojn predikatoj2 "vra" NenombrigeblaKlaso Ĝerundo

   [<TestMethod>]
   member _.SpecifaĜerundo() =
      kontroliInflekciojn difinitoj "vra" NombrigeblaKlaso SpecifaĜerundo
      kontroliInflekciojn difinitoj2 "vra" NenombrigeblaKlaso SpecifaĜerundo
   
   [<TestMethod>]
   member _.Kvalito() =
      kontroliInflekciojn predikatoj "re" NombrigeblaKlaso Kvalito
      kontroliInflekciojn predikatoj2 "re" NenombrigeblaKlaso Kvalito
      
      let _ =
         let [ gremure; gremuro ] = [ "gremure"; "gremuro" ] |> List.map praveMalinflekti
         
         kontroliFrazojn
            "gremure gremuro"
            [ { Kapo = verbo gremuro []
                Argumentoj = [ argumento gremure [] ] } ]
      ()

   [<TestMethod>]
   member _.NevalidajVortoj() =
      difinitoj2
      |> List.map
            ((fun vorto -> vorto + "si")
             >> kontroliNevalidanVorton)
      |> ignore

      difinitoj2
      |> List.map
            ((fun vorto -> vorto + "ve")
             >> kontroliNevalidanVorton)
      |> ignore

      predikatoj
      |> List.map
            ((fun vorto -> vorto + "si")
             >> kontroliNevalidanVorton)
      |> ignore

      predikatoj
      |> List.map
            ((fun vorto -> vorto + "ve")
             >> kontroliNevalidanVorton)
      |> ignore

      predikatoj2
      |> List.map
            ((fun vorto -> vorto + "si")
             >> kontroliNevalidanVorton)
      |> ignore

      predikatoj2
      |> List.map
            ((fun vorto -> vorto + "ve")
             >> kontroliNevalidanVorton)
      |> ignore

      predikatoj
      |> List.map
            ((fun vorto -> vorto + "ga")
             >> kontroliNevalidanVorton)
      |> ignore

      predikatoj
      |> List.map
            ((fun vorto -> vorto + "va")
             >> kontroliNevalidanVorton)
      |> ignore

   [<TestMethod>]
   member _.PlurajInflekcioj() =
      Assert.Inconclusive()
      
//      "kunalasmea"
//      |> kontroliĈiujnInfleckiojn [ Nebazo(NetransitivaVerbo, Ĝerundo, "kunalas")
//                                    Nebazo(NenombrigeblaKlaso, Translativo, "kuna")
//                                    Bazo(NenombrigeblaKlaso, Difinito, "kuna") ]