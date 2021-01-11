namespace KrestiaVortilo.Testo

open KrestiaVortilo
open Microsoft.VisualStudio.TestTools.UnitTesting
open Sintaksanalizilo2
open Testiloj

[<TestClass>]
type AntaŭaModifantoTestoj() =
   
   [<TestMethod>]
   member _.KlasajModifantoj() =
      let [ ser; sona; vor; metilmo; vilka ] =
         [ "ser"; "sona"; "vor"; "Metilmo"; "vilka" ]
         |> List.map praveMalinflekti
         
      kontroliArgumentojn
         "ser sona vor Metilmo vilka"
         [ argumento vilka [ modifanto ser [ nombro [ sona ] 7m ]; modifanto vor [ argumento metilmo [] ] ] ]