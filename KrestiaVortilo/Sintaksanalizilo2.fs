namespace KrestiaVortilo

open FSharpx.Collections
open Malinflektado

module Sintaksanalizilo2 =
   type Verbo = Verbo of MalinflektitaVorto

   type Argumento = Argumento of MalinflektitaVorto

   type Predikato =
      | Predikato1 of predikataVorto: Verbo * argumento1: Argumento
      | Predikato2 of predikataVorto: Verbo * argumento1: Argumento * argumento2: Argumento

   type Sintaksanalizilo =
      { Argumentoj: Deque<Argumento>
        Verboj: Deque<Verbo> }

   type AnaziloRezulto =
      { Frazoj: Predikato list
        RestantajVortoj: MalinflektitaVorto list }

   let kreiSintaksanalizilon =
      { Argumentoj = Deque.empty
        Verboj = Deque.empty }
      
   let kreiRezulton = { Frazoj = []; RestantajVortoj = [] }

   let kategorigi sintaksanalizilo vortoj =
      vortoj
      |> List.fold (fun sintaksanaliziloAk sekvaVorto ->
            match sintaksanaliziloAk with
            | Ok (sintaksanalizilo) ->
               if ĉuPredikataVorto sekvaVorto then
                  { sintaksanalizilo with Verboj = sintaksanalizilo.Verboj.Conj(Verbo sekvaVorto) } |> Ok
               elif ĉuArgumentaVorto sekvaVorto then
                  { sintaksanalizilo with Argumentoj = sintaksanalizilo.Argumentoj.Conj(Argumento sekvaVorto) } |> Ok
               else
                  Error(sprintf "Ne povas kategorigi %s" sekvaVorto.BazaVorto)
            | Error (_) -> sintaksanaliziloAk) (Ok sintaksanalizilo)

   let rec legiFrazojn (sintaksanalizilo: Sintaksanalizilo) (rezulto: AnaziloRezulto) =
      match Deque.tryHead sintaksanalizilo.Verboj with
      | Some sekvaVerbo ->
         match sekvaVerbo with
         | Verbo (vorto) ->
            valencoDe vorto.BazaVorto
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
                        | 1 -> Predikato1(sekvaVerbo, argumentoj.Item 0) |> Ok
                        | _ -> Error (sprintf "Ne povas legi frazon por %s de valencon %d" vorto.BazaVorto valenco))
                     |> Result.bind (fun frazo ->
                        { rezulto with Frazoj = frazo :: rezulto.Frazoj }
                        |> legiFrazojn { Verboj = sintaksanalizilo.Verboj.Tail; Argumentoj = restantaj }))
            |> Option.defaultValue (Error(sprintf "Ne konas la valencon de %s" vorto.BazaVorto))
      | None ->
         { rezulto with
              RestantajVortoj =
                 Deque.toSeq sintaksanalizilo.Argumentoj
                 |> Seq.map (fun argumento ->
                       match argumento with
                       | Argumento (vorto) -> vorto)
                 |> List.ofSeq }
         |> Ok

   let analizi (eniro: string): Result<AnaziloRezulto, string> =
      eniro.Split(' ')
      |> List.ofArray
      |> tuteMalinflektiĈiujn
      |> Result.bind (kategorigi kreiSintaksanalizilon)
      |> Result.bind (fun sintaksanalizilo ->
         let rezulto = kreiRezulton
         legiFrazojn sintaksanalizilo rezulto)