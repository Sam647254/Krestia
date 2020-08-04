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
      ()

   [<TestMethod>]
   member _.NenombrigeblaKontroliTesto() =
      [ ("kitigro", Infinitivo)
        ("kitigre", Difinito)
        ("kitigrensa", Havaĵo)
        ("kitigrowa", PredikativoEsti)
        ("kitigroga", AtributivoEstiMalantaŭ)
        ("kitigrelas", Translativo)
        ("kitigreva", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliInflekcion AntaŭNenombrigeblaEco inflekcio vorto)
      |> ignore

   [<TestMethod>]
   member _.NombrigeblaKontroliTesto() =
      [ ("edro", Infinitivo)
        ("edre", Difinito)
        ("edresi", UnuNombro)
        ("edreve", PluraNombro)
        ("edresinsa", UnuHavaĵo)
        ("edrevensa", PluraHavaĵo)
        ("edrensa", Havaĵo)
        ("edrowa", PredikativoEsti)
        ("edroga", AtributivoEstiMalantaŭ)
        ("edrelas", Translativo)
        ("edrerim", Ekzistado)
        ("edresirim", UnuEkzistado)
        ("edreverim", PluraEkzistado)
        ("edreva", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliInflekcion AntaŭNombrigeblaEco inflekcio vorto)
      |> ignore
