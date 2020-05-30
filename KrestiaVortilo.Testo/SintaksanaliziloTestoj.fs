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
         let prava = Predikato1(Verbo(belise), plenaArgumento (hem))
         kontroliUnuFrazon "hem belise" prava

      let _ =
         let belitri = praveMalinflekti "belitre"
         let hes = praveMalinflekti "hes"
         let lipa = praveMalinflekti "lipa"
         let prava = Predikato2(Verbo(belitri), plenaArgumento (hes), plenaArgumento (lipa))
         kontroliUnuFrazon "belitre hes lipa" prava

      let _ =
         let vortoj = [ "lipa"; "hem"; "het"; "belipro" ] |> List.map praveMalinflekti
         match vortoj with
         | [ lipa; hem; het; belipro ] ->
            let prava = Predikato3(Verbo(belipro), plenaArgumento (lipa), plenaArgumento (hem), plenaArgumento (het))
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
               [ Predikato1(Verbo(belisela), plenaArgumento (lipa))
                 Predikato2(Verbo(belitela), plenaArgumento (het), plenaArgumento (hime))
                 Predikato0(Verbo(belimio)) ]
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
   member _.Pridirantoj() =
      let _ =
         let vortoj = [ "lipa"; "lidea" ] |> List.map praveMalinflekti
         match vortoj with
         | [ lipa; lidea ] ->
            kontroliRestantajnVortojn "lipa lidea" [ Argumento(lipa, [ Modifanto(lidea) ]) ]
         | _ -> failwith "Invalid state"

      let _ =
         let vortoj = [ "lidra"; "lipa" ] |> List.map praveMalinflekti
         match vortoj with
         | [ lidra; lipa ] ->
            kontroliRestantajnVortojn "lidra lipa" [ Argumento(lipa, [ Modifanto(lidra) ]) ]
         | _ -> failwith "Invalid state"

      ()

   [<TestMethod>]
   member _.PlurvortajPlenaArgumentoj() =
      let _ =
         let vortoj = [ "lipa"; "het" ] |> List.map praveMalinflekti
         match vortoj with
         | [ lipa; het ] ->
            kontroliRestantajnVortojn "lipa vol het" [ Plurvorto(plenaArgumento (lipa), plenaArgumento (het), Vol) ]
         | _ -> failwith "Invalid state"

      let _ =
         let vortoj = [ "lipa"; "gremi"; "hem" ] |> List.map praveMalinflekti
         match vortoj with
         | [ lipa; gremi; hem ] ->
            kontroliRestantajnVortojn "lipa vol gremi vol hem"
               [ Plurvorto(plenaArgumento (lipa), Plurvorto(plenaArgumento (gremi), plenaArgumento (hem), Vol), Vol) ]
         | _ -> failwith "Invalid state"

      ()

   [<TestMethod>]
   member _.RepetitajVortoj() =
      let _ =
         let vorto = praveMalinflekti "lipa"
         kontroliRestantajnVortojn "lipa lipa lipa" [ plenaArgumento vorto ]
      
      ()