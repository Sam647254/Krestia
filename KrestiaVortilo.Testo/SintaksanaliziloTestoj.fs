namespace KrestiaVortilo.Testo

open KrestiaVortilo.Sintaksanalizilo2
open Microsoft.VisualStudio.TestTools.UnitTesting

open Testiloj

[<TestClass>]
type Sintaksanalizilo() =

   [<TestMethod>]
   member _.``hem belitre gremi``() =
      // belitre(hem, gremi)
      kontroliKategorigadon "hem belitre gremi" [ "belitre" ] [ "hem"; "gremi" ]

   [<TestMethod>]
   member _.MalplenigitajFormoj() =
      [ "belim" ] |> kontroliMalplenigitajnFormojn "belim"

      [ "belis"; "belim" ] |> kontroliMalplenigitajnFormojn "belis"

      [ "belit"; "belis"; "belig"; "belim" ] |> kontroliMalplenigitajnFormojn "belit"

      [ "belish"; "belis"; "belin"; "belim" ] |> kontroliMalplenigitajnFormojn "belish"

      [ "belip"; "belit"; "belish"; "beliv"; "belis"; "belig"; "belin"; "belim" ]
      |> kontroliMalplenigitajnFormojn "belip"

      [ "belig"; "belim" ] |> kontroliMalplenigitajnFormojn "belig"

      [ "beliv"; "belig"; "belin"; "belim" ] |> kontroliMalplenigitajnFormojn "beliv"

      [ "belin"; "belim" ] |> kontroliMalplenigitajnFormojn "belin"
      
      ()

   [<TestMethod>]
   member _.RestantajVortoj() =
      [ "lipa"; "het" ]
      |> List.map (praveMalinflekti >> plenaArgumento)
      |> kontroliRestantajnVortojn "lipa het"

      kontroliRestantajnVortojn "hem belisela" []
      kontroliRestantajnVortojn "hem belitre hes" []
      kontroliRestantajnVortojn "belitri hes hem"
         [ "hem"
           |> praveMalinflekti
           |> plenaArgumento ]

   [<TestMethod>]
   member _.RepetitajVortoj() =
      let _ =
         let vorto = praveMalinflekti "lipa"
         kontroliRestantajnVortojn "lipa lipa lipa" [ plenaArgumento vorto ]

      ()

   [<TestMethod>]
   member _.NevalidajFrazoj() =
      [ "bet"; "lipa betre"; "sonol lipa hem"; "sonol" ]
      |> List.map kontroliNevalidanFrazon
      |> ignore