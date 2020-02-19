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
        ("omoneri", DativoVolo)
        ("omonema", Ĝerundo)
        ("omonom", PartaDativo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto PartaNetransitivaVerbo inflekcio)
      |> ignore