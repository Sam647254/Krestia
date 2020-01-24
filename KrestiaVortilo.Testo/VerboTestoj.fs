namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting
open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type VerboTestoj () =
   
   [<TestMethod>]
   member _.Infinitivoj() =
      [ "morem"; "gelum"; "seskom"; "kirim" ]
      |> List.map (kontroliInflekcion MalplenaVerbo Infinitivo)
      |> ignore
      
      [ "liveras"; "bemos"; "emeras"; "kemis" ]
      |> List.map (kontroliInflekcion NetransitivaVerbo Infinitivo)
      |> ignore
      
      [ "nitrit"; "buvitot"; "dliret"; "klitret" ]
      |> List.map (kontroliInflekcion TransitivaVerbo Infinitivo)
      |> ignore
      
      [ "eramatosh"; "tesh"; "volesh"; "vilish" ]
      |> List.map (kontroliInflekcion NedirektaTransitivaVerbo Infinitivo)
      |> ignore
    
   [<TestMethod>]
   member _.NevalidajVortoj() =
      [ "m"; "s"; "t"; "sh"; "p"; "n"; "eg"; "ig" ]
      |> List.map kontroliNevalidanVorton
      |> ignore