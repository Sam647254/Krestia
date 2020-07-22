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

   [<TestMethod>]
   member _.PredikatoTestoj() =
      let _ =
         let belise = praveMalinflekti "belise"
         let hem = praveMalinflekti "hem"
         let prava = Predikato1(plenaVerbo (belise), plenaArgumento (hem))
         kontroliUnuFrazon "hem belise" prava

      let _ =
         let belitri = praveMalinflekti "belitre"
         let hes = praveMalinflekti "hes"
         let lipa = praveMalinflekti "lipa"
         let prava = Predikato2(plenaVerbo (belitri), plenaArgumento (hes), plenaArgumento (lipa))
         kontroliUnuFrazon "belitre hes lipa" prava

      let _ =
         let vortoj = [ "lipa"; "hem"; "het"; "belipro" ] |> List.map praveMalinflekti
         match vortoj with
         | [ lipa; hem; het; belipro ] ->
            let prava =
               Predikato3(plenaVerbo (belipro), plenaArgumento (lipa), plenaArgumento (hem), plenaArgumento (het))
            kontroliUnuFrazon "lipa hem het belipro" prava
         | _ -> Assert.Fail "Invalid state"

      ()

   [<TestMethod>]
   member _.PlurajFrazojTestoj() =
      let _ =
         let vortoj = [ "lipa"; "belisela"; "het"; "belitela"; "hime"; "belimio" ] |> List.map praveMalinflekti
         match vortoj with
         | [ lipa; belisela; het; belitela; hime; belimio ] ->
            let prava =
               [ Predikato1(plenaVerbo (belisela), plenaArgumento (lipa))
                 Predikato2(plenaVerbo (belitela), plenaArgumento (het), plenaArgumento (hime))
                 Predikato0(plenaVerbo (belimio)) ]
            kontroliPlurajnFrazojn "lipa belisela het belitela hime belimio" prava []
         | _ -> Assert.Fail "Invalid state"
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

   [<TestMethod>]
   member _.``hem tatretowa``() =
      let vortoj = [ "hem"; "tatretowa" ] |> List.map praveMalinflekti
      match vortoj with
      | [ hem; tatretowa ] ->
         kontroliUnuFrazon "hem tatretowa" (Predikato1(plenaVerbo tatretowa, plenaArgumento hem))
      ()
