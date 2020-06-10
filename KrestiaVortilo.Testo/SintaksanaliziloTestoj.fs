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
   member _.``vilka sinil vol Metilmo``() =
      let _ =
         let vortoj = [ "gremi"; "Metilmo" ] |> List.map praveMalinflekti
         match vortoj with
         | [ gremi; metilmo ] ->
            kontroliRestantajnVortojn "gremi vol Metilmo"
               [ Plurvorto(plenaArgumento gremi, plenaArgumento metilmo, Vol) ]
         | _ -> failwith "Invalid state"

      let _ =
         let tlatira = praveMalinflekti "Tlatira"
         kontroliRestantajnVortojn "Tlatira" [ plenaArgumento tlatira ]

      let _ =
         let alano = praveMalinflekti "Alano"
         kontroliRestantajnVortojn "Alano" [ plenaArgumento alano ]

      let _ =
         let vortoj = [ "delta"; "krenodea"; "lite" ] |> List.map praveMalinflekti
         match vortoj with
         | [ delta; krenodea; lite ] ->
            kontroliRestantajnVortojn "delta krenodea mel lite"
               [ Argumento
                  (delta,
                   [ Pridiranto(krenodea)
                     Mel(plenaArgumento lite) ]
                   |> Set.ofList) ]
         | _ -> failwith "Invalid state"

      let _ =
         let vortoj = [ "rinome"; "seskoma" ] |> List.map praveMalinflekti
         match vortoj with
         | [ rinome; seskoma ] ->
            kontroliRestantajnVortojn "rinome sonol seskoma"
               [ Argumento(rinome, [ Sonol(plenaArgumento seskoma) ] |> Set.ofList) ]
         | _ -> failwith "Invalid state"

      let _ =
         let alanokonal = praveMalinflekti "Alanokonal"
         kontroliRestantajnVortojn "Alanokonal" [ plenaArgumento alanokonal ]

      let _ =
         let vortoj = [ "rote"; "Nivernal" ] |> List.map praveMalinflekti
         match vortoj with
         | [ rote; nivernal ] ->
            kontroliRestantajnVortojn "rote nal Nivernal"
               [ Plurvorto(plenaArgumento rote, plenaArgumento nivernal, Nal) ]
         | _ -> failwith "Invalid state"

      ()

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
   member _.Pridirantoj() =
      let _ =
         let vortoj = [ "lipa"; "lidea" ] |> List.map praveMalinflekti
         match vortoj with
         | [ lipa; lidea ] ->
            kontroliRestantajnVortojn "lipa lidea" [ Argumento(lipa, Set.singleton (Pridiranto(lidea))) ]
         | _ -> failwith "Invalid state"

      let _ =
         let vortoj = [ "lidra"; "lipa" ] |> List.map praveMalinflekti
         match vortoj with
         | [ lidra; lipa ] ->
            kontroliRestantajnVortojn "lidra lipa" [ Argumento(lipa, Set.singleton (Pridiranto(lidra))) ]
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

   [<TestMethod>]
   member _.NevalidajFrazoj() =
      [ "bet"; "lipa betre"; "sonol lipa hem"; "sonol" ]
      |> List.map kontroliNevalidanFrazon
      |> ignore

   [<TestMethod>]
   member _.Nevil() =
      let _ =
         let vortoj = [ "hem"; "bese" ] |> List.map praveMalinflekti
         match vortoj with
         | [ hem; bese ] ->
            kontroliUnuFrazon "hem bese nevil"
               (Predikato1(Verbo(bese, Set.singleton (plenaModifanto "nevil")), plenaArgumento hem))
      ()

   [<TestMethod>]
   member _.``hem tatretowa``() =
      let vortoj = [ "hem"; "tatretowa" ] |> List.map praveMalinflekti
      match vortoj with
      | [ hem; tatretowa ] ->
         kontroliUnuFrazon "hem tatretowa" (Predikato1(plenaVerbo tatretowa, plenaArgumento hem))
      ()
