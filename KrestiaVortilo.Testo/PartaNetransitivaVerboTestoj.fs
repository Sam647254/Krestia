namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type PartaNetransitivaVerboTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("omon", Infinitivo)
        ("omonia", Progresivo)
        ("omonio", Perfekto)
        ("omonela", Estonteco)
        ("omoneri", Ujo3Volo)
        ("omonea", Ĝerundo)
        ("omonom", PartaUjo3) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliInflekcion NedirektaNetransitivaVerbo inflekcio vorto)
      |> ignore