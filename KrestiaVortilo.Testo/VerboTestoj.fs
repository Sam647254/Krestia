namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting
open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type VerboTestoj () =
   
   [<TestMethod>]
   member _.Infinitivoj() =
      [ "morem"; "gelum"; "seskom"; "kirem" ]
      |> List.map (kontroliInflekcion MalplenaVerbo Infinitivo)
      |> ignore
      
      [ "liveras"; "bemos"; "emeras"; "kemis" ]
      |> List.map (kontroliInflekcion NetransitivaVerbo Infinitivo)
      |> ignore