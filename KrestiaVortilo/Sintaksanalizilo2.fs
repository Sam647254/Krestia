namespace KrestiaVortilo

open FSharpx.Collections
open Malinflektado

module Sintaksanalizilo2 =
   type Verbo = Verbo of MalinflektitaVorto

   type Modifanto =
      | Modifanto of MalinflektitaVorto
      | Mel of Argumento
      | Sonol of Argumento

   and Parvorto =
      | Vol
      | Del
      | Nal

   and Argumento =
      | Argumento of MalinflektitaVorto * Modifanto list
      | Plurvorto of nukleo: Argumento * modifanto: Argumento * parvorto: Parvorto

   type Predikato =
      | Predikato0 of predikatoVorto: Verbo
      | Predikato1 of predikataVorto: Verbo * argumento1: Argumento
      | Predikato2 of predikataVorto: Verbo * argumento1: Argumento * argumento2: Argumento
      | Predikato3 of predikataVorto: Verbo * argumento1: Argumento * argumento2: Argumento * argumento3: Argumento

   type AtendantaPlurvorto =
      | AtendantaParvorto of argumento: Argumento * parvorto: Parvorto
      | AtendantaModifanto of Modifanto

   type Sintaksanalizilo =
      { Argumentoj: Deque<Argumento>
        Verboj: Deque<Verbo>
        AtendantajFrazoj: (Argumento -> Argumento) list
        LastaArgumento: Argumento option }

   type AnaziloRezulto =
      { Frazoj: Predikato list
        RestantajVortoj: Argumento list }
   
   let parvortoj =
      [ "vol", Vol
        "del", Del
        "nal", Nal ]
      |> Map.ofList

   let plenaArgumento vorto = Argumento(vorto, [])

   let kreiSintaksanalizilon =
      { Argumentoj = Deque.empty
        Verboj = Deque.empty
        AtendantajFrazoj = []
        LastaArgumento = None }

   let kreiRezulton =
      { Frazoj = []
        RestantajVortoj = [] }

   let rec aldoniModifanton argumento modifanto =
      match argumento with
      | Argumento (a, modifantoj) -> Argumento(a, modifanto :: modifantoj)
      | Plurvorto (nukleo, subvorto, parvorto) -> Plurvorto(aldoniModifanton nukleo modifanto, subvorto, parvorto)

   let lastaArgumentoDe sintaksanalizilo =
      match sintaksanalizilo.LastaArgumento with
      | Some (a) -> Ok (a, { sintaksanalizilo with LastaArgumento = None })
      | None -> Error "Mankas lasta argumento"

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
      { purigita with LastaArgumento = Some argumento }

   let anstataŭigiLastanArgumenton sintaksanalizilo argumento =
      { sintaksanalizilo with LastaArgumento = Some argumento }
      
   let forigiRepetajnVortojn vortoj =
      vortoj
      |> List.fold
            (fun ak sek -> if List.isEmpty ak || sek <> List.head ak then sek :: ak else ak) []
      |> List.rev

   let kategorigi sintaksanalizilo vortoj =
      vortoj
      |> List.fold (fun sintaksanaliziloAk sekvaVorto ->
            match sintaksanaliziloAk with
            | Ok (sintaksanalizilo) ->
               if ĉuPredikataVorto sekvaVorto then
                  { sintaksanalizilo with Verboj = sintaksanalizilo.Verboj.Conj(Verbo sekvaVorto) } |> Ok
               elif ĉuArgumentaVorto sekvaVorto then
                  aldoniArgumenton sintaksanalizilo (Argumento(sekvaVorto, [])) |> Ok
               elif ĉuMalantaŭModifantaVorto sekvaVorto then
                  lastaArgumentoDe sintaksanalizilo
                  |> Result.map (fun (lastaArgumento, sintaksanalizilo) ->
                        let novaArgumento = aldoniModifanton lastaArgumento (Modifanto(sekvaVorto))
                        anstataŭigiLastanArgumenton sintaksanalizilo novaArgumento)
               elif ĉuAntaŭModifantaVorto sekvaVorto then
                  { sintaksanalizilo with
                       AtendantajFrazoj =
                          (fun argumento ->
                             match argumento with
                             | Argumento (a, modifantoj) -> Argumento(a, Modifanto(sekvaVorto) :: modifantoj)
                             | Plurvorto (nukleo, modifanto, parvorto) ->
                                Plurvorto(aldoniModifanton nukleo (Modifanto(sekvaVorto)), modifanto, parvorto))
                          :: sintaksanalizilo.AtendantajFrazoj }
                  |> Ok
               elif Map.containsKey sekvaVorto.BazaVorto parvortoj then
                  let parvorto = parvortoj.[sekvaVorto.BazaVorto]
                  lastaArgumentoDe sintaksanalizilo
                  |> Result.map (fun (lastaArgumento, sintaksanalizilo) ->
                        { sintaksanalizilo with
                             AtendantajFrazoj =
                                (fun argumento -> Plurvorto(lastaArgumento, argumento, parvorto))
                                :: sintaksanalizilo.AtendantajFrazoj })
               else
                  Error(sprintf "Ne povas kategorigi %s" sekvaVorto.BazaVorto)
            | Error (_) -> sintaksanaliziloAk) (Ok sintaksanalizilo)
      |> Result.map purigiLastanArgumenton

   let rec legiFrazojn (sintaksanalizilo: Sintaksanalizilo) (rezulto: AnaziloRezulto) =
      match Deque.tryHead sintaksanalizilo.Verboj with
      | Some sekvaVerbo ->
         match sekvaVerbo with
         | Verbo (vorto) ->
            valencoDe vorto
            |> Option.map (fun valenco ->
                  if sintaksanalizilo.Argumentoj.Length < valenco then
                     Error(sprintf "Ne sufiĉe da argumentoj por %s" vorto.BazaVorto)
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
                        | 2 -> Predikato2(sekvaVerbo, argumentoj.Item 0, argumentoj.Item 1) |> Ok
                        | 3 -> Predikato3(sekvaVerbo, argumentoj.Item 0, argumentoj.Item 1, argumentoj.Item 2) |> Ok
                        | _ -> Error(sprintf "Ne povas legi frazon por %s de valencon %d" vorto.BazaVorto valenco))
                     |> Result.bind (fun frazo ->
                           { rezulto with Frazoj = frazo :: rezulto.Frazoj }
                           |> legiFrazojn
                                 { Verboj = sintaksanalizilo.Verboj.Tail
                                   Argumentoj = restantaj
                                   AtendantajFrazoj = sintaksanalizilo.AtendantajFrazoj
                                   LastaArgumento = sintaksanalizilo.LastaArgumento }))
            |> Option.defaultValue (Error(sprintf "Ne konas la valencon de %s" vorto.BazaVorto))
      | None ->
         { rezulto with
              RestantajVortoj = Deque.toSeq sintaksanalizilo.Argumentoj |> List.ofSeq
              Frazoj = List.rev rezulto.Frazoj }
         |> Ok

   let analizi (eniro: string): Result<AnaziloRezulto, string> =
      eniro.Split(' ')
      |> List.ofArray
      |> forigiRepetajnVortojn
      |> tuteMalinflektiĈiujn
      |> Result.bind (kategorigi kreiSintaksanalizilon)
      |> Result.bind (fun sintaksanalizilo ->
            let rezulto = kreiRezulton
            legiFrazojn sintaksanalizilo rezulto)
