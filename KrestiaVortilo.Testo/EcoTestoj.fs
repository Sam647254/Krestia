namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type EcoTestoj () =

   [<TestMethod>]
   member _.NenombrigeblaKontroliTesto() =
      [ ("kitigro", Infinitivo)
        ("kitigre", Difinito)
        ("kitigrensa", Havaĵo)
        ("kitigrowa", PredikativoEsti)
        ("kitigroga", AtributativoEsti)
        ("kitigrelas", Translativo)
        ("kitigreva", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto NenombrigeblaEco inflekcio)
      |> ignore

   [<TestMethod>]
   member _.NombrigeblaKostroliTesto () =
      [ ("edro", Infinitivo)
        ("edre", Difinito)
        ("edresi", UnuNombro)
        ("edreve", PluraNombro)
        ("edresinsa", UnuHavaĵo)
        ("edrevensa", PluraHavaĵo)
        ("edrensa", Havaĵo)
        ("edrowa", PredikativoEsti)
        ("edroga", AtributativoEsti)
        ("edrelas", Translativo)
        ("edrerim", Ekzistado)
        ("edresirim", Ekzistado)
        ("edreverim", Ekzistado)
        ("edreva", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto NombrigeblaEco inflekcio)
      |> ignore