namespace KrestiaVortilo

open FSharpx.Collections
open Malinflektado

module Sintaksanalizilo2 =
   type Verbo = Verbo of MalinflektitaVorto * Modifantoj: Set<PredikataVerboModifantajVorto>

   and ArgumentaModifanto =
      | Pridiranto of MalinflektitaVorto
      | Mel of Argumento
      | Sonol of Argumento
      | Nival

   and PredikataVerboModifanto = | Nevil

   and PredikataVerboModifantajVorto =
      { Modifanto: PredikataVerboModifanto
        Vorto: EniraVorto }

   and Parvorto =
      | Vol
      | Del
      | Nal

   and Argumento =
      | Argumento of MalinflektitaVorto * Set<ArgumentaModifanto>
      | Plurvorto of nukleo: Argumento * modifanto: Argumento * parvorto: Parvorto

   type Predikato =
      | Predikato0 of predikatoVorto: Verbo
      | Predikato1 of predikataVorto: Verbo * argumento1: Argumento
      | Predikato2 of predikataVorto: Verbo * argumento1: Argumento * argumento2: Argumento
      | Predikato3 of predikataVorto: Verbo * argumento1: Argumento * argumento2: Argumento * argumento3: Argumento

   type AtendantaPlurvorto =
      | AtendantaParvorto of argumento: Argumento * parvorto: Parvorto
      | AtendantaModifanto of ArgumentaModifanto

   type Sintaksanalizilo =
      { Argumentoj: Deque<Argumento>
        Verboj: Deque<Verbo>
        AtendantajFrazoj: (Argumento -> Argumento) list
        KonstruontajModifantoj: (EniraVorto * (Argumento -> Sintaksanalizilo -> Result<Sintaksanalizilo, Eraro>)) list
        LastaArgumento: Argumento option }

   type Analizejo =
      { Argumentoj: Deque<Argumento>
        Frazoj: Predikato list
        RestantajVortoj: MalinflektitaVorto list }

   type AnaziloRezulto =
      { Frazoj: Predikato list
        RestantajVortoj: Argumento list }

   let parvortoj =
      [ "vol", Vol; "del", Del; "nal", Nal ]
      |> Map.ofList

   let modifantoj1DeKlasoj =
      [ "mel", Mel; "sonol", Sonol ] |> Map.ofList

   let modifantojDePredikataVerboj = [ "nevil", Nevil ] |> Map.ofList

   let plenaArgumento vorto = Argumento(vorto, Set.empty)

   let plenaVerbo vorto = Verbo(vorto, Set.empty)

   let kreiSintaksanalizilon =
      { Argumentoj = Deque.empty
        Verboj = Deque.empty
        AtendantajFrazoj = []
        LastaArgumento = None
        KonstruontajModifantoj = [] }

   let kreiRezulton = { Frazoj = []; RestantajVortoj = [] }

   let rec aldoniModifanton argumento modifanto =
      match argumento with
      | Argumento (a, modifantoj) -> Argumento(a, modifantoj |> Set.add modifanto)
      | Plurvorto (nukleo, subvorto, parvorto) -> Plurvorto(aldoniModifanton nukleo modifanto, subvorto, parvorto)

   let lastaArgumentoDe sintaksanalizilo =
      sintaksanalizilo.LastaArgumento
      |> Option.map (fun a ->
            (a,
             { sintaksanalizilo with
                  LastaArgumento = None }))

   let purigiLastanArgumenton sintaksanalizilo =
      match sintaksanalizilo.LastaArgumento with
      | Some (lastaArgumento) ->
         if sintaksanalizilo.AtendantajFrazoj.IsEmpty then
            { sintaksanalizilo with
                 Argumentoj = sintaksanalizilo.Argumentoj.Conj(lastaArgumento)
                 LastaArgumento = None }
         else
            let modifitaArgumento =
               sintaksanalizilo.AtendantajFrazoj
               |> List.fold (fun ak sekva -> sekva ak) lastaArgumento

            { sintaksanalizilo with
                 Argumentoj = sintaksanalizilo.Argumentoj.Conj(modifitaArgumento)
                 AtendantajFrazoj = []
                 LastaArgumento = None }
      | None -> sintaksanalizilo

   let aldoniArgumenton sintaksanalizilo argumento =
      let purigita = purigiLastanArgumenton sintaksanalizilo
      { purigita with
           LastaArgumento = Some argumento }

   let anstataŭigiLastanArgumenton sintaksanalizilo argumento =
      { sintaksanalizilo with
           LastaArgumento = Some argumento }

   let aldoniModifantonAlLastaVerbo sintaksanalizilo modifanto: Result<Sintaksanalizilo, Eraro> =
      sintaksanalizilo.Verboj.TryLast
      |> Option.map (fun lastaVerbo ->
            let novaVerbo =
               match lastaVerbo with
               | Verbo (v, listo) -> Verbo(v, Set.add modifanto listo)

            { sintaksanalizilo with
                 Verboj = sintaksanalizilo.Verboj.Initial.Conj novaVerbo })
      |> Option.map Ok
      |> Option.defaultValue (Error((modifanto.Vorto, sprintf "Neniu verbo por la modifanto %A" modifanto)))

   let forigiRepetajnVortojn (vortoj: EniraVorto list): EniraVorto list =
      vortoj
      |> List.fold<EniraVorto, EniraVorto list> (fun ak sek ->
            if List.isEmpty ak
               || sek.Vorto
               <> (List.head ak |> (fun v -> v.Vorto)) then
               sek :: ak
            else
               ak) []
      |> List.rev

   let finiKategorigadon (sintaksanalizilo: Sintaksanalizilo) =
      if sintaksanalizilo.KonstruontajModifantoj.Length > 0 then
         let (vorto, _) =
            sintaksanalizilo.KonstruontajModifantoj.Head

         Error(vorto, sprintf "%s needs additional helping words" vorto.Vorto)
      else
         Ok sintaksanalizilo

   let kategorigi sintaksanalizilo vortoj: Result<Sintaksanalizilo, Eraro> =
      vortoj
      |> List.fold (fun sintaksanaliziloAk sekvaVorto ->
            match sintaksanaliziloAk with
            | Ok (sintaksanalizilo) ->
               if ĉuPredikataVorto sekvaVorto then
                  { sintaksanalizilo with
                       Verboj = sintaksanalizilo.Verboj.Conj(plenaVerbo sekvaVorto) }
                  |> Ok
               elif ĉuArgumentaVorto sekvaVorto then
                  match sintaksanalizilo.KonstruontajModifantoj with
                  | (_, sekva) :: restantaj ->
                     { sintaksanalizilo with
                          KonstruontajModifantoj = restantaj }
                     |> sekva (plenaArgumento sekvaVorto)
                  | _ ->
                     aldoniArgumenton sintaksanalizilo (Argumento(sekvaVorto, Set.empty))
                     |> Ok
               elif ĉuMalantaŭModifantaVorto sekvaVorto then
                  lastaArgumentoDe sintaksanalizilo
                  |> Option.map (fun (lastaArgumento, sintaksanalizilo) ->
                        let novaArgumento =
                           aldoniModifanton lastaArgumento (Pridiranto(sekvaVorto))

                        anstataŭigiLastanArgumenton sintaksanalizilo novaArgumento
                        |> Ok)
                  |> Option.defaultValue
                        (Error
                           ((sekvaVorto.OriginalaVorto,
                             (sprintf "%s has nothing to modify" sekvaVorto.OriginalaVorto.Vorto))))
               elif ĉuAntaŭModifantaVorto sekvaVorto then
                  { sintaksanalizilo with
                       AtendantajFrazoj =
                          (fun argumento ->
                             match argumento with
                             | Argumento (a, modifantoj) -> Argumento(a, modifantoj |> Set.add (Pridiranto sekvaVorto))
                             | Plurvorto (nukleo, modifanto, parvorto) ->
                                Plurvorto(aldoniModifanton nukleo (Pridiranto(sekvaVorto)), modifanto, parvorto))
                          :: sintaksanalizilo.AtendantajFrazoj }
                  |> Ok
               elif Map.containsKey sekvaVorto.BazaVorto parvortoj then
                  let parvorto = parvortoj.[sekvaVorto.BazaVorto]
                  lastaArgumentoDe sintaksanalizilo
                  |> Option.map (fun (lastaArgumento, sintaksanalizilo) ->
                        { sintaksanalizilo with
                             AtendantajFrazoj =
                                (fun argumento -> Plurvorto(lastaArgumento, argumento, parvorto))
                                :: sintaksanalizilo.AtendantajFrazoj })
                  |> Option.map Ok
                  |> Option.defaultValue
                        (Error((sekvaVorto.OriginalaVorto, (sprintf "No head for %s" sekvaVorto.OriginalaVorto.Vorto))))
               else
                  match sekvaVorto.BazaVorto with
                  | _ when modifantoj1DeKlasoj
                           |> Map.containsKey sekvaVorto.BazaVorto ->
                     let modifanto =
                        Map.find sekvaVorto.BazaVorto modifantoj1DeKlasoj

                     { sintaksanalizilo with
                          KonstruontajModifantoj =
                             (sekvaVorto.OriginalaVorto,
                              (fun argumento novaSA ->
                                 lastaArgumentoDe novaSA
                                 |> Option.map (fun (lastaArgumento, novaSA) ->
                                       modifanto argumento
                                       |> aldoniModifanton lastaArgumento
                                       |> aldoniArgumenton novaSA)
                                 |> Option.map Ok
                                 |> Option.defaultValue
                                       (Error
                                          (sekvaVorto.OriginalaVorto,
                                           sprintf "%s has nothing to modify" sekvaVorto.OriginalaVorto.Vorto))))
                             :: sintaksanalizilo.KonstruontajModifantoj }
                     |> Ok
                  | _ when modifantojDePredikataVerboj
                           |> Map.containsKey sekvaVorto.BazaVorto ->
                     aldoniModifantonAlLastaVerbo
                        sintaksanalizilo
                        { Modifanto = Map.find sekvaVorto.BazaVorto modifantojDePredikataVerboj
                          Vorto = sekvaVorto.OriginalaVorto }
                  | _ -> Error((sekvaVorto.OriginalaVorto, sprintf "Ne povas kategorigi %s" sekvaVorto.BazaVorto))
            | Error (_) -> sintaksanaliziloAk) (Ok sintaksanalizilo)
      |> Result.map purigiLastanArgumenton
      |> Result.bind finiKategorigadon

   let rec legiFrazojn (sintaksanalizilo: Sintaksanalizilo) (rezulto: AnaziloRezulto): Result<AnaziloRezulto, Eraro> =
      match Deque.tryHead sintaksanalizilo.Verboj with
      | Some sekvaVerbo ->
         match sekvaVerbo with
         | Verbo (vorto, _) ->
            valencoDe vorto
            |> Option.map (fun valenco ->
                  if sintaksanalizilo.Argumentoj.Length < valenco then
                     Error((vorto.OriginalaVorto, sprintf "Ne sufiĉe da argumentoj por %s" vorto.BazaVorto))
                  else
                     let (argumentoj, restantaj) =
                        seq { 1 .. valenco }
                        |> Seq.fold (fun (listo, restantaj) _ ->
                              let (a, novaRestantaj) = Deque.uncons restantaj
                              (a :: listo, novaRestantaj)) ([], sintaksanalizilo.Argumentoj)

                     argumentoj
                     |> List.rev
                     |> (fun argumentoj ->
                        match valenco with
                        | 0 -> Predikato0(sekvaVerbo) |> Ok
                        | 1 -> Predikato1(sekvaVerbo, argumentoj.Item 0) |> Ok
                        | 2 ->
                           Predikato2(sekvaVerbo, argumentoj.Item 0, argumentoj.Item 1)
                           |> Ok
                        | 3 ->
                           Predikato3(sekvaVerbo, argumentoj.Item 0, argumentoj.Item 1, argumentoj.Item 2)
                           |> Ok
                        | _ ->
                           Error
                              ((vorto.OriginalaVorto,
                                sprintf "Ne povas legi frazon por %s de valencon %d" vorto.BazaVorto valenco)))
                     |> Result.bind (fun frazo ->
                           { rezulto with
                                Frazoj = frazo :: rezulto.Frazoj }
                           |> legiFrazojn
                                 { Verboj = sintaksanalizilo.Verboj.Tail
                                   Argumentoj = restantaj
                                   AtendantajFrazoj = sintaksanalizilo.AtendantajFrazoj
                                   LastaArgumento = sintaksanalizilo.LastaArgumento
                                   KonstruontajModifantoj = sintaksanalizilo.KonstruontajModifantoj }))
            |> Option.defaultValue (Error(vorto.OriginalaVorto, (sprintf "Ne konas la valencon de %s" vorto.BazaVorto)))
      | None ->
         { rezulto with
              RestantajVortoj =
                 Deque.toSeq sintaksanalizilo.Argumentoj
                 |> List.ofSeq
              Frazoj = List.rev rezulto.Frazoj }
         |> Ok

   let iĝiEnEnirajVortoj ĉuTesto (eniro: string) =
      eniro.Split('\n')
      |> List.ofArray
      |> List.map (fun vico -> vico.Split(' '))
      |> List.map List.ofArray
      |> List.mapi (fun vico vortojDeVico ->
            vortojDeVico
            |> List.fold (fun (pozo, ak) sekva ->
                  (pozo + String.length sekva + 1,
                   { Vico = if ĉuTesto then 0 else vico
                     Pozo = if ĉuTesto then 0 else pozo
                     Vorto = sekva }
                   :: ak)) (0, [])
            |> fun (_, listo) -> listo
            |> List.rev)
      |> List.concat

   let analizi (eniro: string) ĉuTesto: Result<AnaziloRezulto, Eraro> =
      eniro
      |> iĝiEnEnirajVortoj ĉuTesto
      |> forigiRepetajnVortojn
      |> tuteMalinflektiĈiujn
      |> Result.bind (kategorigi kreiSintaksanalizilon)
      |> Result.bind (fun sintaksanalizilo ->
            let rezulto = kreiRezulton
            legiFrazojn sintaksanalizilo rezulto)

   let legiPridiranton (analizejo: Analizejo) = failwith "???"

   let proviLegiArgumentajnModifantojn (restantajVortoj: MalinflektitaVorto list) =
      let modifantoj =
         restantajVortoj
         |> List.takeWhile ĉuMalantaŭModifantaVorto

      let restantaj =
         restantajVortoj |> List.skip modifantoj.Length

      (modifantoj |> List.map Pridiranto |> Set.ofList, restantaj)

   let legiArgumenton (analizejo: Analizejo) =
      let argumento = analizejo.RestantajVortoj.Head

      let modifantoj, restantaj =
         proviLegiArgumentajnModifantojn analizejo.RestantajVortoj.Tail

      { analizejo with
           Argumentoj = analizejo.Argumentoj.Conj(Argumento(argumento, modifantoj))
           RestantajVortoj = restantaj } |> Ok

   let aldoniMalantaŭanModifanton (analizejo: Analizejo) =
      let modifanto = analizejo.RestantajVortoj.Head
      let lastaArgumento = analizejo.Argumentoj.Last
      let restantaj = analizejo.RestantajVortoj.Tail

      let novaArgumento =
         match lastaArgumento with
         | Argumento (argumento, modifantoj) -> Argumento(argumento, modifantoj.Add(Pridiranto modifanto))
         | _ -> failwith "WIP"

      { analizejo with
           RestantajVortoj = restantaj
           Argumentoj = analizejo.Argumentoj.Initial.Conj(novaArgumento) }
      |> Ok

   let legiSekvan (analizejo: Analizejo) =
      match analizejo.RestantajVortoj with
      | sekva :: _ ->
         if ĉuArgumentaVorto sekva then
            legiArgumenton analizejo
         elif ĉuMalantaŭModifantaVorto sekva then
            aldoniMalantaŭanModifanton analizejo
         else
            Eraro(sekva.OriginalaVorto, sprintf "Can't parse %s" sekva.OriginalaVorto.Vorto)
            |> Error
      | [] -> Ok analizejo

   let legiPerAnalizejo (vortoj: MalinflektitaVorto list) =
      let analizejo =
         { RestantajVortoj = vortoj
           Frazoj = []
           Argumentoj = Deque.empty }

      legiSekvan analizejo

   let legi (eniro: string) ĉuTesto: Result<Analizejo, Eraro> =
      eniro
      |> iĝiEnEnirajVortoj ĉuTesto
      |> forigiRepetajnVortojn
      |> tuteMalinflektiĈiujn
      |> Result.bind legiPerAnalizejo
