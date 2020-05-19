namespace KrestiaVortilo

open FSharpx.Collections
open Malinflektado

module Sintaksanalizilo2 =
   type Verbo = Verbo of MalinflektitaVorto

   type Modifanto = Modifanto of MalinflektitaVorto

   type Argumento =
      | Argumento of MalinflektitaVorto * Modifanto list
      | Vol of nukleo: Argumento * modifanto: Argumento

   type Predikato =
      | Predikato0 of predikatoVorto: Verbo
      | Predikato1 of predikataVorto: Verbo * argumento1: Argumento
      | Predikato2 of predikataVorto: Verbo * argumento1: Argumento * argumento2: Argumento
      | Predikato3 of predikataVorto: Verbo * argumento1: Argumento * argumento2: Argumento * argumento3: Argumento

   type Sintaksanalizilo =
      { Argumentoj: Deque<Argumento>
        Verboj: Deque<Verbo>
        AtendantaModifantoj: Modifanto list
        AtendantaVol: Argumento option
        LastaArgumento: Argumento option }

   type AnaziloRezulto =
      { Frazoj: Predikato list
        RestantajVortoj: Argumento list }
   
   let plenaArgumento vorto = Argumento(vorto, [])

   let kreiSintaksanalizilon =
      { Argumentoj = Deque.empty
        Verboj = Deque.empty
        AtendantaModifantoj = []
        AtendantaVol = None
        LastaArgumento = None }

   let kreiRezulton =
      { Frazoj = []
        RestantajVortoj = [] }

   let rec aldoniModifanton argumento modifanto =
      match argumento with
      | Argumento (a, modifantoj) -> Argumento(a, modifanto :: modifantoj)
      | Vol (nukleo, subvorto) -> Vol(aldoniModifanton nukleo modifanto, subvorto)

   let aldoniArgumenton sintaksanalizilo argumento =
      match sintaksanalizilo.LastaArgumento with
      | Some (lastaArgumento) ->
         { sintaksanalizilo with
              Argumentoj = sintaksanalizilo.Argumentoj.Conj(lastaArgumento)
              LastaArgumento = Some argumento }
      | None ->
         { sintaksanalizilo with LastaArgumento = Some argumento }

   let aldoniArgumentonKunAtendantajModifantoj sintaksanalizilo argumento =
      let novaArgumento = Argumento(argumento, sintaksanalizilo.AtendantaModifantoj)
      { aldoniArgumenton sintaksanalizilo novaArgumento with AtendantaModifantoj = [] }

   let kategorigi sintaksanalizilo vortoj =
      vortoj
      |> List.fold (fun sintaksanaliziloAk sekvaVorto ->
            match sintaksanaliziloAk with
            | Ok (sintaksanalizilo) ->
               if ĉuPredikataVorto sekvaVorto then
                  { sintaksanalizilo with Verboj = sintaksanalizilo.Verboj.Conj(Verbo sekvaVorto) } |> Ok
               elif ĉuArgumentaVorto sekvaVorto then
                  if List.isEmpty sintaksanalizilo.AtendantaModifantoj
                  then aldoniArgumenton sintaksanalizilo (Argumento(sekvaVorto, [])) |> Ok
                  else aldoniArgumentonKunAtendantajModifantoj sintaksanalizilo sekvaVorto |> Ok
               elif ĉuMalantaŭModifantaVorto sekvaVorto then
                  let lastaArgumento = sintaksanalizilo.Argumentoj.Last

                  let novaArgumento = aldoniModifanton lastaArgumento (Modifanto(sekvaVorto))
                  { sintaksanalizilo with Argumentoj = sintaksanalizilo.Argumentoj.Initial.Conj novaArgumento } |> Ok
               elif ĉuAntaŭModifantaVorto sekvaVorto then
                  { sintaksanalizilo with AtendantaModifantoj =
                       Modifanto(sekvaVorto) :: sintaksanalizilo.AtendantaModifantoj } |> Ok
               elif sekvaVorto.BazaVorto = "vol" then
                  let lastaArgumento = sintaksanalizilo.Argumentoj.Last
                  { sintaksanalizilo with AtendantaVol = Some lastaArgumento } |> Ok
               else
                  Error(sprintf "Ne povas kategorigi %s" sekvaVorto.BazaVorto)
            | Error (_) -> sintaksanaliziloAk) (Ok sintaksanalizilo)

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
                                   AtendantaModifantoj = sintaksanalizilo.AtendantaModifantoj
                                   AtendantaVol = sintaksanalizilo.AtendantaVol
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
      |> tuteMalinflektiĈiujn
      |> Result.bind (kategorigi kreiSintaksanalizilon)
      |> Result.bind (fun sintaksanalizilo ->
            let rezulto = kreiRezulton
            legiFrazojn sintaksanalizilo rezulto)
