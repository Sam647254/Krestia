namespace KrestiaVortilo.Testo

open KrestiaVortilo
open Microsoft.VisualStudio.TestTools.UnitTesting
open Sintaksanalizilo2
open Testiloj

[<TestClass>]
type AntaŭaModifantoTestoj() =
   
   [<TestMethod>]
   member _.KlasajModifantoj() =
      let [ ser; vor; metilmo; vilka ] =
         [ "ser"; "vor"; "Metilmo"; "vilka" ]
         |> List.map praveMalinflekti
         
      kontroliArgumentojn
         "ser sona vor Metilmo vilka"
         [ argumento vilka [ modifanto ser [ nombro 7m ]; modifanto vor [ argumento metilmo [] ] ] ]