namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type NenombrigeblaKlasoTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("gremu", Infinitivo)
        ("gremi", NekonitaNombro) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto NenombrigeblaKlaso inflekcio)
      |> ignore