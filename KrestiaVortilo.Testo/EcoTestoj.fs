namespace KrestiaVortilo.Testo

open KrestiaVortilo
open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Sintaksanalizilo2
open KrestiaVortilo.Imperativa
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

         kontroliArgumentojn
            "dedre krena"
            [ plenaModifitaArgumento dedre (List.singleton (EcoDe(argumento krena []))) ]

      let _ =
         let [ krena; dedri ] =
            [ "krena"; "dedri" ] |> List.map praveMalinflekti

         kontroliArgumentojn
            "krena dedri"
            [ plenaModifitaArgumento dedri (List.singleton (EcoDe(argumento krena []))) ]

      ()
      
   [<TestMethod>]
   member _.PredikativoEsti() =
      kontroliInflekcion AntaŭNombrigeblaEco PredikativoEsti "dedro"
      kontroliInflekcion AntaŭNenombrigeblaEco PredikativoEsti "amegro"
      kontroliInflekcion MalantaŭNombrigeblaEco PredikativoEsti "dedru"
      kontroliInflekcion MalantaŭNenombrigeblaEco PredikativoEsti "amegru"
      
      let _ =
         let [ krena; dedri; dedru ] =
            [ "krena"; "dedri"; "dedru" ] |> List.map praveMalinflekti
         
         kontroliFrazojn "krena dedri krena dedru"
            [ { Kapo = verbo dedru [ EcoDe (argumento krena []) ]
                Argumentoj = [ argumento dedri [ EcoDe (argumento krena []) ] ] } ]
      ()
   
   [<TestMethod>]
   member _.UnuNombro() =
      kontroliInflekcion AntaŭNombrigeblaEco UnuNombro "dedresi"
      kontroliInflekcion MalantaŭNombrigeblaEco UnuNombro "dedrisi"