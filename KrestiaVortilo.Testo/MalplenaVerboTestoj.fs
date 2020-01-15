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
        ("meratremela", Estonteco)
        ("meratremilim", Translativo)
        ("meratremena", Ĝerundo) ]
      |> List.map (fun (vorto, inflekcio) -> kontroliFormon vorto MalplenaVerbo inflekcio)
      |> ignore