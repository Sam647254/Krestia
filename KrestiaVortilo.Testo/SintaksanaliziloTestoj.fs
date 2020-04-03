namespace KrestiaVortilo.Testo

open Microsoft.VisualStudio.TestTools.UnitTesting

open Testiloj

[<TestClass>]
type Sintaksanalizilo() =

   [<TestMethod>]
   member _.``hem meratre gremi``() =
      // meratre(hem, gremi)
      kontroliKategorigadon "hem meratre gremi" [ "meratre" ] [ "hem"; "gremi" ]
      
   [<TestMethod>]
   member _.MalplenigitajFormoj () =
      [ "morem" ]
      |> kontroliMalplenigitajnFormojn "morem"
      
      [ "liveras"; "liveram" ]
      |> kontroliMalplenigitajnFormojn "liveras"
      
      [ "emat"; "emas"; "emag"; "emam" ]
      |> kontroliMalplenigitajnFormojn "emat"
      
      [ "erash"; "eras"; "eran"; "eram" ]
      |> kontroliMalplenigitajnFormojn "erash"
      
      [ "trirep"; "triret"; "trires"; "triresh"; "trirev"; "trireg"; "triren"; "trirem" ]
      |> kontroliMalplenigitajnFormojn "trirep"
      
      [ "aleg"; "alem" ]
      |> kontroliMalplenigitajnFormojn "aleg"
      
      [ "serav"; "serag"; "seran" ]
      |> kontroliMalplenigitajnFormojn "serav"
      
      [ "prevan"; "prevam" ]
      |> kontroliMalplenigitajnFormojn "prevan"