namespace KrestiaVortilo.Testo

open KrestiaVortilo.Testo.Testiloj
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type PredikataAŭArgumentaTestoj() =
   
   [<TestMethod>]
   member _.PredikataVortoj() =
      [ "merat"
        "buvitotri"
        "liverasela"
        "nitritre"
        "gremirimia"
        "kunarise"
        "verikevera"
        "trirepro" ]
      |> List.map kontroliĈuPredikata
      |> ignore
      
   [<TestMethod>]
   member _.ArgumentaVortoj() =
      [ "kuna"
        "gremi"
        "livosmea"
        "kirimelimea"
        "verikeve"
        "lepasi"
        "tetavra" ]
      |> List.map kontroliĈuArgumenta
      |> ignore