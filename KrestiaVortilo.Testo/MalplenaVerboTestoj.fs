namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open KrestiaVortilo.Vorttipo
open Testiloj

[<TestClass>]
type MalplenaVerboTestoj () =

   [<TestMethod>]
   member _.KontroliTesto () =
      [ ("meratrem", Infinitivo)
        ("meratremia", Progresivo)
        ("meratremio", Perfekto)
        ("meratremela", Intenco)
        ("meratremelim", Translativo)
        ("meratremea", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliInflekcion MalplenaVerbo inflekcio vorto)
      |> ignore