namespace KrestiaVortilo.Testo

open KrestiaVortilo
open Microsoft.VisualStudio.TestTools.UnitTesting
open KrestiaVortilo.Vorttipo
open KrestiaVortilo.Sintaksanalizilo
open Testiloj

[<TestClass>]
type VerboTestoj() =

   [<TestMethod>]
   member _.Infinitivoj() =
      [ "morem"
        "gelum"
        "seskom"
        "kirim"
        "liveras"
        "bemos"
        "emeras"
        "kemis"
        "nitrit"
        "buvitot"
        "dliret"
        "klitret"
        "eramatosh"
        "tesh"
        "volesh"
        "vilish" ]
      |> List.map Malinflektado.ĉuVerbo
      |> List.map (fun rezulto ->
            match rezulto with
            | Ok(_) -> ()
            | Error((_, eraro)) -> Assert.Fail(eraro))
      |> ignore

      [ "morem"; "gelum"; "seskom"; "kirim" ]
      |> List.map (kontroliInflekcion MalplenaVerbo Progresivo)
      |> ignore

      [ "liveras"; "bemos"; "emeras"; "kemis" ]
      |> List.map (kontroliInflekcion NetransitivaVerbo Progresivo)
      |> ignore

      [ "nitrit"; "buvitot"; "dliret"; "klitret" ]
      |> List.map (kontroliInflekcion TransitivaVerbo Progresivo)
      |> ignore

      [ "eramatosh"; "tesh"; "volesh"; "vilish" ]
      |> List.map (kontroliInflekcion NedirektaTransitivaVerbo Progresivo)
      |> ignore

   [<TestMethod>]
   member _.NevalidajVortoj() =
      [ "m"; "s"; "t"; "sh"; "p"; "n"; "g"; "v"; "tetio" ]
      |> List.map kontroliNevalidanVorton
      |> ignore

   [<TestMethod>]
   member _.PlurajInflekcioj() =
      Assert.Inconclusive()
      
      "meratonialasela"
      |> kontroliĈiujnInfleckiojn
            [ Nebazo(NetransitivaVerbo, Intenco, "meratonialas")
              Nebazo(NombrigeblaKlaso, Translativo, "meratoniaa")
              Nebazo(TransitivaVerbo, Argumento2, "merat")
              Bazo(TransitivaVerbo, Progresivo, "merat") ]

      "liverasetie"
      |> kontroliĈiujnInfleckiojn
            [ Nebazo(NombrigeblaKlaso, Difinito, "liverasetio")
              Nebazo(NetransitivaVerbo, Argumento1, "liveras")
              Bazo(NetransitivaVerbo, Progresivo, "liveras") ]
