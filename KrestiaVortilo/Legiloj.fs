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
            | (Pridiranto, AtributativoEsti) ->
               legiModifantojnAk (Modifanto0(malinflektita) :: listo) restantaj vortajModifantoj
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
               | _ -> Error (sprintf "%s ne havas validan inflekcion" unua)
               |> Result.bind (fun (vorto, restantaj) ->
                  legiModifantojnAk [] restantaj vortajModifantoj
                  |> Result.map (fun (modifantoj, restantaj) ->
                     if modifantoj.IsEmpty
                     then (vorto, restantaj)
                     else (PridiritaVorto(vorto, modifantoj), restantaj)))
            | None -> Error (sprintf "%s estas nevalida" unua)
         | [] -> Error "Ankoraŭ bezonas vortojn, sed ne plu estas"
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
   
   let legiPredikaton
      (partoj: string list) (valencoDe: string -> int option) vortajModifantoj =
      match partoj with
      | unua :: restantaj ->
         match malinflekti unua with
         | Some(vortformoj, malinflektita) ->
            match List.last vortformoj with
            | (TransitivaVerbo, inflekcio) ->
               valencoDe malinflektita
               |> Option.map (fun valenco ->
                  { Verbo = malinflektita; Inflekcio = inflekcio }
                  |> legiTransitivanPredikaton valenco restantaj vortajModifantoj)
               |> Option.defaultValue (Error (sprintf "ne eblas trovi la valencon de %s" malinflektita))
            | _ -> Error (sprintf "ne eblas komenci predikaton per %s" malinflektita)
         | None -> Error (sprintf "%s estas nevalida" unua)
      | [] -> Error (sprintf "bezonas pli da vorton")