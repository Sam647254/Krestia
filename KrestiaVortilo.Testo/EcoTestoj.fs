namespace KrestiaVortilo.Testo

open KrestiaVortilo
open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Sintaksanalizilo2
open KrestiaVortilo.Imperativa
open Microsoft.VisualStudio.TestTools.UnitTesting
open Testiloj

[<TestClass>]
type EcoTestoj() =

   [<TestMethod>]
   member _.Difinito() =
      kontroliInflekcion AntaŭNombrigeblaEco Difinito "dedre"
      kontroliInflekcion AntaŭNenombrigeblaEco Difinito "amegre"
      kontroliInflekcion MalantaŭNombrigeblaEco Difinito "dedri"
      kontroliInflekcion MalantaŭNenombrigeblaEco Difinito "amegri"

      let _ =
         let [ dedre; krena ] =
            [ "dedre"; "krena" ] |> List.map praveMalinflekti

         kontroliArgumentojn "dedre krena" [ plenaModifitaArgumento dedre (List.singleton (EcoDe(argumento krena []))) ]

      let _ =
         let [ krena; dedri ] =
            [ "krena"; "dedri" ] |> List.map praveMalinflekti

         kontroliArgumentojn "krena dedri" [ plenaModifitaArgumento dedri (List.singleton (EcoDe(argumento krena []))) ]

      ()

   [<TestMethod>]
   member _.PredikativoEsti() =
      kontroliInflekcion AntaŭNombrigeblaEco PredikativoEsti "dedro"
      kontroliInflekcion AntaŭNenombrigeblaEco PredikativoEsti "amegro"
      kontroliInflekcion MalantaŭNombrigeblaEco PredikativoEsti "dedru"
      kontroliInflekcion MalantaŭNenombrigeblaEco PredikativoEsti "amegru"

      let _ =
         let [ krena; dedri; dedru ] =
            [ "krena"; "dedri"; "dedru" ]
            |> List.map praveMalinflekti

         kontroliFrazojn
            "krena dedri krena dedru"
            [ { Kapo = verbo dedru [ EcoDe(argumento krena []) ]
                Argumentoj = [ argumento dedri [ EcoDe(argumento krena []) ] ] } ]

      ()

   [<TestMethod>]
   member _.UnuNombro() =
      kontroliInflekcion AntaŭNombrigeblaEco UnuNombro "dedresi"
      kontroliInflekcion MalantaŭNombrigeblaEco UnuNombro "dedrisi"

   [<TestMethod>]
   member _.PluraNombro() =
      kontroliInflekcion AntaŭNombrigeblaEco PluraNombro "dedreve"
      kontroliInflekcion MalantaŭNombrigeblaEco PluraNombro "dedrive"

   [<TestMethod>]
   member _.Havaĵo() =
      [ "nsa", Havaĵo
        "sinsa", UnuHavaĵo
        "vensa", PluraHavaĵo ]
      |> List.map (fun (finaĵo, inflekcio) ->
            kontroliInflekcion AntaŭNombrigeblaEco inflekcio ("dedre" + finaĵo)
            kontroliInflekcion MalantaŭNombrigeblaEco inflekcio ("dedri" + finaĵo))
      |> ignore

      kontroliInflekcion AntaŭNenombrigeblaEco Havaĵo "amegrensa"
      kontroliInflekcion MalantaŭNenombrigeblaEco Havaĵo "amegrinsa"

      let _ =
         let [ amegrensa; gremu ] =
            [ "amegrensa"; "gremu" ]
            |> List.map praveMalinflekti

         kontroliFrazojn
            "amegrensa gremu"
            [ { Kapo = verbo gremu []
                Argumentoj = [ argumento amegrensa [] ] } ]

      ()

   [<TestMethod>]
   member _.Fokuso() =
      [ "le", Fokuso
        "sile", UnuFokuso
        "vele", PluraFokuso ]
      |> List.map (fun (finaĵo, inflekcio) ->
            kontroliInflekcion AntaŭNombrigeblaEco inflekcio ("dedre" + finaĵo)
            kontroliInflekcion MalantaŭNombrigeblaEco inflekcio ("dedri" + finaĵo))
      |> ignore

      kontroliInflekcion AntaŭNenombrigeblaEco Fokuso "amegrele"
      kontroliInflekcion MalantaŭNenombrigeblaEco Fokuso "amegrile"

   [<TestMethod>]
   member this.AtributivoEstiMalantaŭ() =
      this.TestiFinaĵon "ga" AtributivoEstiMalantaŭ

      let _ =
         let [ kuna; amegre; seskoma ] =
            [ "kuna"; "amegrega"; "seskoma" ]
            |> List.map praveMalinflekti

         kontroliArgumentojn
            "kuna amegrega seskoma"
            [ argumento kuna [ Pridiranto(argumento amegre [ EcoDe(argumento seskoma []) ]) ] ]

      let _ =
         let [ kuna; amegre; seskoma; kunaga ] =
            [ "kuna"
              "amegrega"
              "seskoma"
              "kunaga" ]
            |> List.map praveMalinflekti

         kontroliArgumentojn
            "kuna amegrega seskoma kunaga"
            [ argumento
               kuna
                 [ Pridiranto(argumento amegre [ EcoDe(argumento seskoma [ Pridiranto(argumento kunaga []) ]) ]) ] ]

      let _ =
         let [ kuna; seskoma; amegriga ] =
            [ "kuna"; "seskoma"; "amegriga" ]
            |> List.map praveMalinflekti

         kontroliArgumentojn
            "kuna seskoma amegriga"
            [ argumento kuna [ Pridiranto(argumento amegriga [ EcoDe(argumento seskoma []) ]) ] ]

      let _ =
         let [ kuna; gremiva; seskoma; kunaga; amegriga ] =
            [ "kuna"
              "gremiva"
              "seskoma"
              "kunaga"
              "amegriga" ]
            |> List.map praveMalinflekti

         kontroliArgumentojn
            "kuna gremiva seskoma kunaga amegriga"
            [ argumento
               kuna
                 [ Pridiranto
                    (argumento
                       amegriga
                        [ EcoDe
                           (argumento
                              seskoma
                               [ Pridiranto(argumento gremiva [])
                                 Pridiranto(argumento kunaga []) ]) ]) ] ]

      ()

   [<TestMethod>]
   member this.AtributivoEstiAntaŭ() =
      this.TestiFinaĵon "va" AtributivoEstiAntaŭ

   [<TestMethod>]
   member this.Havado() = this.TestiFinaĵon "res" Havado

   [<TestMethod>]
   member this.Ekzistado() = this.TestiFinaĵon "rim" Ekzistado

   [<TestMethod>]
   member this.Translativo() = this.TestiFinaĵon2 "las" Translativo

   [<TestMethod>]
   member this.Ĝerundo() = this.TestiFinaĵon2 "vra" Ĝerundo

   [<TestMethod>]
   member this.SpecifaĜerundo() = this.TestiFinaĵon "vra" SpecifaĜerundo

   [<TestMethod>]
   member this.Kvalito() = this.TestiFinaĵon2 "re" Kvalito

   [<TestMethod>]
   member _.AtributivoEstiLegado() =
      let _ =
         let [ amegreva; seskoma; kuna ] =
            [ "amegreva"; "seskoma"; "kuna" ]
            |> List.map praveMalinflekti

         kontroliArgumentojn
            "amegreva seskoma kuna"
            [ argumento kuna [ Pridiranto(argumento amegreva [ EcoDe(argumento seskoma []) ]) ] ]

      let _ =
         let [ amegre; seskoma; kunaga; kuna ] =
            [ "amegreva"
              "seskoma"
              "kunaga"
              "kuna" ]
            |> List.map praveMalinflekti

         kontroliArgumentojn
            "amegreva seskoma kunaga kuna"
            [ argumento
               kuna
                 [ Pridiranto(argumento amegre [ EcoDe(argumento seskoma [ Pridiranto(argumento kunaga []) ]) ]) ] ]

      ()

   member private _.TestiFinaĵon finaĵo inflekcio =
      kontroliInflekcion AntaŭNombrigeblaEco inflekcio ("dedre" + finaĵo)
      kontroliInflekcion AntaŭNenombrigeblaEco inflekcio ("amegre" + finaĵo)
      kontroliInflekcion MalantaŭNombrigeblaEco inflekcio ("dedri" + finaĵo)
      kontroliInflekcion MalantaŭNenombrigeblaEco inflekcio ("amegri" + finaĵo)

   member private _.TestiFinaĵon2 finaĵo inflekcio =
      kontroliInflekcion AntaŭNombrigeblaEco inflekcio ("dedro" + finaĵo)
      kontroliInflekcion AntaŭNenombrigeblaEco inflekcio ("amegro" + finaĵo)
      kontroliInflekcion MalantaŭNombrigeblaEco inflekcio ("dedru" + finaĵo)
      kontroliInflekcion MalantaŭNenombrigeblaEco inflekcio ("amegru" + finaĵo)
