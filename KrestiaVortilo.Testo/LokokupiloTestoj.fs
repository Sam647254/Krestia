namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type LokokupiloTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ "wel"
        "wil"
        "hem"
        "hes"
        "het"
        "heti"
        "heta"
        "hime"
        "hise"
        "hite"
        "hiti"
        "hita"
        "hori"
        "hemse"
        "wen"
        "won" ]
      |> List.map (fun vorto -> kontroliInflekcion Lokokupilo Difinito vorto)
      |> ignore