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
        ("kitigroga", AtributivoEstiMalantaŭ)
        ("kitigrelas", Translativo)
        ("kitigreva", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto AntaŭNenombrigeblaEco inflekcio)
      |> ignore

   [<TestMethod>]
   member _.NombrigeblaKontroliTesto () =
      [ ("edro", Infinitivo)
        ("edre", Difinito)
        ("edresi", UnuNombro)
        ("edreve", PluraNombro)
        ("edresinsa", UnuHavaĵo)
        ("edrevensa", PluraHavaĵo)
        ("edrensa", Havaĵo)
        ("edrowa", PredikativoEsti)
        ("edroga", AtributivoEstiMalantaŭ)
        ("edrelas", Translativo)
        ("edrerim", Ekzistado)
        ("edresirim", UnuEkzistado)
        ("edreverim", PluraEkzistado)
        ("edreva", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto AntaŭNombrigeblaEco inflekcio)
      |> ignore