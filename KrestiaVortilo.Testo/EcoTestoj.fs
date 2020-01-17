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

   [<TestMethod>]
   member _.NombrigeblaKonstroliTesto () =
      [ ("endro", Infinitivo)
        ("endre", NekonitaNombro)
        ("endresi", UnuNombro)
        ("endreve", PluraNombro)
        ("endresinsa", Havaĵo)
        ("endrevensa", Havaĵo)
        ("endrensa", Havaĵo)
        ("endrowa", PredikativoEsti)
        ("endroga", AtributativoEsti)
        ("endrelas", Translativo)
        ("endrerim", Ekzistado)
        ("endresirim", Ekzistado)
        ("endreverim", Ekzistado)
        ("endreva", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto NombrigeblaEco inflekcio)
      |> ignore