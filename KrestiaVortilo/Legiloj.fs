namespace KrestiaVortilo

open Vorttipo
open Traktilaro
open Strukturo

module Legiloj =
   let rec legiModifantojnAk
      (listo: Modifanto list) (vortoj: string list) (vortajModifantoj: Set<string>) =
      match vortoj with
      | unua :: restantaj ->
         match malinflekti unua with
         | Some(vortformoj, malinflektita) ->
            let lastaFormo = List.last vortformoj
            match lastaFormo with
            | (SintaksaVorto, SolaFormo) ->
               if vortajModifantoj.Contains(malinflektita)
               then legiModifantojnAk (Modifanto0(malinflektita) :: listo) restantaj vortajModifantoj
               else Error (sprintf "La sintaksa vorto %s ne estas modifanto" malinflektita)
            | (Vorttipo.Pridiranto, AtributativoEsti) ->
               legiModifantojnAk (Pridiranto(malinflektita) :: listo) restantaj vortajModifantoj
            | _ -> (listo, vortoj) |> Ok
         | None -> Error (sprintf "%s ne estas modifanto" unua)
      | [] -> (listo, vortoj) |> Ok

   let rec legiObjektojn (kvanto: int) (listo: string list) vortajModifantoj =
      if kvanto = 0
      then ([], listo) |> Ok
      else
         match listo with
         | unua :: restantaj ->
            match malinflekti unua with
            | Some(vortformoj, malinflektita) ->
               let lastaFormo = List.last vortformoj
               match lastaFormo with
               | (Vorttipo.Lokokupilo, SolaFormo) -> (Lokokupilo(malinflektita), restantaj) |> Ok
               | (Vorttipo.FremdaVorto, SolaFormo) -> (FremdaVorto(malinflektita), restantaj) |> Ok
               | (NombrigeblaKlaso, inflekcio) ->
                  (Objekto({ Objekto = malinflektita; Inflekcio = inflekcio }), restantaj) |> Ok
               | (NenombrigeblaEco, Havaĵo) ->
                  (Objekto({ Objekto = malinflektita; Inflekcio = Havaĵo}), restantaj) |> Ok
               | (NenombrigeblaEco, inflekcio) ->
                  legiObjektojn 1 restantaj vortajModifantoj
                  |> Result.map (fun (posedantoListo, restantaj) ->
                     let posedanto = posedantoListo.Head
                     (Eco({ Objekto = malinflektita; Inflekcio = inflekcio }, posedanto), restantaj))
               | _ -> Error (sprintf "%s ne havas validan inflekcion por objekto" unua)
               |> Result.bind (fun (vorto, restantaj) ->
                  legiModifantojnAk [] restantaj vortajModifantoj
                  |> Result.map (fun (modifantoj, restantaj) ->
                     if modifantoj.IsEmpty
                     then (vorto, restantaj)
                     else (PridiritaVorto(vorto, modifantoj), restantaj)))
            | None -> Error (sprintf "%s estas nevalida" unua)
         | [] -> Error (sprintf "Ankoraŭ bezonas vortojn (%d), sed ne plu estas" kvanto)
         |> Result.bind (fun (sekva, restantaj) ->
            legiObjektojn (kvanto - 1) restantaj vortajModifantoj
            |> Result.map (fun (restantajObjektoj, restantajPartoj) ->
               (sekva :: restantajObjektoj, restantajPartoj)))

   let legiTransitivanPredikaton 
      (valenco: int) (partoj: string list) vortajModifantoj (verbo: Verbo) =
      legiObjektojn valenco partoj vortajModifantoj
      |> Result.bind (fun objektoj ->
         match objektoj with
         | ([ vorto1; vorto2 ], restantaj) -> (Predikato2(verbo, vorto1, vorto2), restantaj) |> Ok
         | ([ vorto1; vorto2; vorto3 ], restantaj) ->
            (Predikato3(verbo, vorto1, vorto2, vorto3), restantaj) |> Ok
         | _ -> Error (sprintf "%s ne havas valencon de 2 aŭ 3" verbo.Verbo))

   let legiPredikaton (listo: string list) valencoDe vortajModifantoj =
      match listo with
      | unua :: restantaj ->
         match malinflekti unua with
         | Some(vortformoj, malinflektita) ->
            match List.last vortformoj with
            | (TransitivaVerbo, inflekcio) ->
               valencoDe malinflektita
               |> Option.map (fun valenco ->
                  { Verbo = malinflektita; Inflekcio = inflekcio }
                  |> legiTransitivanPredikaton valenco restantaj vortajModifantoj
                  |> Result.map (fun (tp, restantaj) -> (tp, restantaj)))
               |> Option.defaultValue (Error (sprintf "ne eblas trovi la valencon de %s" malinflektita))
            | _ -> Error (sprintf "predikato ne eblas komenci per %s" unua)
         | None -> Error (sprintf "%s estas nevalida vorto" unua)
      | _ -> Error (sprintf "bezonas pli da vortoj por legi predikaton")

   let legiKondicianFrazon (listo: string list) valencoDe vortajModifantoj =
      legiPredikaton listo valencoDe vortajModifantoj
      |> Result.bind
         (fun (kondicio, restantaj) ->
            legiPredikaton restantaj valencoDe vortajModifantoj
            |> Result.map (fun (predikato, r) -> (Peral(kondicio, predikato), r)))
   
   let legiFrazon
      (partoj: string list) (valencoDe: string -> int option) vortajModifantoj =
      match partoj with
      | unua :: restantaj ->
         kontroli unua
         |> Option.map
            (fun vortformo ->
               match vortformo with
               | (TransitivaVerbo, _) | (NetransitivaVerbo, _) ->
                  legiPredikaton partoj valencoDe vortajModifantoj
                  |> Result.map (fun (predikato, restantaj) -> (Predikato(predikato), restantaj))
               | (SintaksaVorto, SolaFormo) ->
                  match unua with
                  | "peral" ->
                     legiKondicianFrazon restantaj valencoDe vortajModifantoj
                  | _ -> Error (sprintf "La sintaksvorto %s ne eblas komenci frazon" unua)
               | _ -> Error (sprintf "%s ne eblas komenci frazon" unua))
         |> Option.defaultValue (Error (sprintf "%s estas nevalida vorto" unua))
      | [] -> Error (sprintf "bezonas pli da vortoj")