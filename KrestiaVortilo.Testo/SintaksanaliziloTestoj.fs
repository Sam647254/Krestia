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
         let prava = Predikato1(Verbo(belise), Argumento(hem))
         kontroliUnuFrazon "hem belise" prava

      let _ =
         let belitri = praveMalinflekti "belitri"
         let hes = praveMalinflekti "hes"
         let lipa = praveMalinflekti "lipa"
         let prava = Predikato2(Verbo(belitri), Argumento(hes), Argumento(lipa))
         kontroliUnuFrazon "belitri hes lipa" prava

      let _ =
         let vortoj = [ "lipa"; "hem"; "het"; "belipro" ] |> List.map praveMalinflekti
         match vortoj with
         | [ lipa; hem; het; belipro ] ->
            let prava = Predikato3(Verbo(belipro), Argumento(lipa), Argumento(hem), Argumento(het))
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
               [ Predikato1(Verbo(belisela), Argumento(lipa))
                 Predikato2(Verbo(belitela), Argumento(het), Argumento(hime))
                 Predikato0(Verbo(belimio)) ]
            kontroliPlurajnFrazojn "lipa belisela het belitela hime belimio" prava []
         | _ -> Assert.Fail "Invalid state"
      ()
