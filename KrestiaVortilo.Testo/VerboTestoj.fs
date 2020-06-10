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
      [ "m"; "s"; "t"; "sh"; "p"; "n"; "g"; "v"; "tetio" ]
      |> List.map kontroliNevalidanVorton
      |> ignore

   [<TestMethod>]
   member _.PlurajInflekcioj() =
      "meratonialasela"
      |> kontroliĈiujnInfleckiojn
            [ Nebazo(NetransitivaVerbo, Estonteco, "meratonialas")
              Nebazo(NombrigeblaKlaso, Translativo, "meratoniaa")
              Nebazo(TransitivaVerbo, Argumento2, "merat")
              Bazo(TransitivaVerbo, Infinitivo, "merat") ]

      "liverasetie"
      |> kontroliĈiujnInfleckiojn
            [ Nebazo(NombrigeblaKlaso, Difinito, "liverasetio")
              Nebazo(NetransitivaVerbo, Argumento1, "liveras")
              Bazo(NetransitivaVerbo, Infinitivo, "liveras") ]
