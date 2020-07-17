namespace KrestiaVortilo.Testo

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
   member _.UnuNombro() =
      kontroliInflekciojn difinitoj "si" NombrigeblaKlaso UnuNombro

   [<TestMethod>]
   member _.PluraNombro() =
      kontroliInflekciojn difinitoj "ve" NombrigeblaKlaso PluraNombro

   [<TestMethod>]
   member _.Havaĵo() =
      kontroliInflekciojn difinitoj "nsa" NombrigeblaKlaso Havaĵo
      kontroliInflekciojn difinitoj "sinsa" NombrigeblaKlaso UnuHavaĵo
      kontroliInflekciojn difinitoj "vensa" NombrigeblaKlaso PluraHavaĵo

      kontroliInflekciojn difinitoj2 "nsa" NenombrigeblaKlaso Havaĵo

   [<TestMethod>]
   member _.Fokuso() =
      kontroliInflekciojn difinitoj "la" NombrigeblaKlaso Fokuso
      kontroliInflekciojn difinitoj "sila" NombrigeblaKlaso UnuFokuso
      kontroliInflekciojn difinitoj "vela" NombrigeblaKlaso PluraFokuso

      kontroliInflekciojn difinitoj2 "la" NenombrigeblaKlaso Fokuso

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
            kontroliArgumentojn "imilta kunataga" [ Argumento(imilta, Set.singleton (Modifanto.Pridiranto kunataga)) ]
         | _ -> Assert.Fail()

      let _ =
         let vortoj =
            List.map praveMalinflekti [ "kunatava"; "imilta" ]

         match vortoj with
         | [ kunatava; imilta ] ->
            kontroliArgumentojn "kunatava imilta" [ Argumento(imilta, Set.singleton (Modifanto.Pridiranto kunatava)) ]
         | _ -> Assert.Fail()

      let _ =
         let vortoj =
            List.map praveMalinflekti [ "kunatava"; "imilta"; "rimaga" ]

         match vortoj with
         | [ kunatava; imilta; rimaga ] ->
            kontroliArgumentojn
               "kunatava imilta rimaga"
               [ Argumento
                  (imilta,
                   Set.ofList [ Modifanto.Pridiranto kunatava
                                Modifanto.Pridiranto rimaga ]) ]
         | _ -> Assert.Fail()

      let _ =
         let vortoj =
            List.map praveMalinflekti [ "kunatava"; "rimava"; "imilta" ]

         match vortoj with
         | [ kunatava; rimava; imilta ] ->
            kontroliArgumentojn
               "kunatava rimava imilta"
               [ Argumento
                  (imilta,
                   Set.ofList [ Modifanto.Pridiranto kunatava
                                Modifanto.Pridiranto rimava ]) ]
         | _ -> Assert.Fail()

      ()

   [<TestMethod>]
   member _.Sola() =
      kontroliInflekciojn difinitoj "ra" NombrigeblaKlaso Sola
      kontroliInflekciojn difinitoj "sira" NombrigeblaKlaso UnuSola
      kontroliInflekciojn difinitoj "vera" NombrigeblaKlaso PluraSola

      kontroliInflekciojn difinitoj2 "ra" NenombrigeblaKlaso Sola

      let _ =
         let imiltara = praveMalinflekti "imiltara"
         kontroliFrazojn "imiltara" [ Predikato0(plenaVerbo imiltara) ]

      let _ =
         match List.map praveMalinflekti [ "imiltara"; "kunataga" ] with
         | [ imiltara; kunataga ] ->
            kontroliFrazojn
               "imiltara kunataga"
               [ Predikato0(Verbo(imiltara, Set.singleton (Modifanto.Pridiranto(kunataga)))) ]
         | _ -> Assert.Fail()
      
      let _ =
         match List.map praveMalinflekti [ "kunatava"; "imiltara" ] with
         | [ kunatava; imiltara ] ->
            kontroliFrazojn
               "kunatava imiltara"
               [ Predikato0(Verbo(imiltara, Set.singleton (Modifanto.Pridiranto(kunatava)))) ]
         | _ -> Assert.Fail()

      ()
      
         
   [<TestMethod>]
   member _.Havado() =
      [ "res", Havado
        "rem", Havado
        "sires", UnuHavado
        "sirem", UnuHavado
        "veres", PluraHavado
        "verem", PluraHavado ]
      |> List.map (fun (finaĵo, inflekcio) -> kontroliInflekciojn difinitoj finaĵo NombrigeblaKlaso inflekcio)
      |> ignore
      
      kontroliInflekciojn difinitoj2 "res" NenombrigeblaKlaso Havado
      kontroliInflekciojn difinitoj2 "rem" NenombrigeblaKlaso Havado
      ()

   [<TestMethod>]
   member _.Estado() =
      [ ("verikowa", PredikativoEsti)
        ("voritoga", AtributivoEstiMalantaŭ)
        ("voritova", AtributivoEstiAntaŭ) ]
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
   member _.Ĝerundo() =
      [ ("verikevra", Ĝerundo)
        ("verikeva", SpecifaĜerundo) ]
      |> List.map (fun (vorto, pravaInflekcio) -> kontroliInflekcion NombrigeblaKlaso pravaInflekcio vorto)
      |> ignore

   [<TestMethod>]
   member _.NevalidajVortoj() =
      [ "kunasi"
        "kunasira"
        "mo"
        "ko"
        "tu"
        "ti"
        "tira" ]
      |> List.map kontroliNevalidanVorton
      |> ignore

      kontroliInflekcion NetransitivaVerbo Infinitivo "kunaveris"
      |> ignore

   [<TestMethod>]
   member _.PlurajInflekcioj() =
      "kinarimela"
      |> kontroliĈiujnInfleckiojn [ Nebazo(MalplenaVerbo, Estonteco, "kinarim")
                                    Nebazo(NenombrigeblaKlaso, Ekzistado, "kinaa")
                                    Bazo(NenombrigeblaKlaso, Infinitivo, "kinaa") ]

      "kunalasmea"
      |> kontroliĈiujnInfleckiojn [ Nebazo(NetransitivaVerbo, Ĝerundo, "kunalas")
                                    Nebazo(NenombrigeblaKlaso, Translativo, "kunaa")
                                    Bazo(NenombrigeblaKlaso, Infinitivo, "kunaa") ]

      "nekeveregore"
      |> kontroliĈiujnInfleckiojn [ Nebazo(OblikaNetransitivaVerbo, Ujo2Volo, "nekevereg")
                                    Nebazo(NombrigeblaKlaso, PluraHavado, "neko")
                                    Bazo(NombrigeblaKlaso, Infinitivo, "neko") ]
