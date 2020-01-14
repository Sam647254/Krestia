namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type EcoTestoj () =

   [<TestMethod>]
   member _.NenombrigeblaKontroliTesto() =
      [ ("kitigro", Infinitivo)
        ("kitigre", NekonitaNombro)
        ("kitigrensa", Havaĵo)
        ("kitigrowa", PredikativoEsti)
        ("kitigroga", AtributativoEsti)
        ("kitigrelas", Translativo)
        ("kitigreva", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto NenombrigeblaEco inflekcio)
      |> ignore