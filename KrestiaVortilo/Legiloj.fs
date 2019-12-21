namespace KrestiaVortilo

open Vorttipo
open Traktilaro
open Strukturo

module Legiloj =
   let rec legiModifantojnAk
      (listo: Modifanto list) (vortoj: string list) (vortajModifantoj: Set<string>) (lastaVorto: string option) =
      match vortoj with
      | unua :: restantaj ->
         match malinflekti unua with
         | Some(vortformoj, malinflektita) ->
            let lastaFormo = List.last vortformoj
            match lastaFormo with
            | (SintaksaVorto, SolaFormo) ->
               match malinflektita with
               | "vol" ->
                  legiObjekto restantaj vortajModifantoj (Some("vol"))
                  |> Result.bind
                     (fun (volObjekto, restantaj, lastaVorto) ->
                        legiModifantojnAk (Modifanto1(malinflektita, volObjekto) :: listo)
                           restantaj vortajModifantoj lastaVorto)
               | _ when vortajModifantoj.Contains(malinflektita) ->
                  legiModifantojnAk (Modifanto0(malinflektita) :: listo) restantaj vortajModifantoj (Some(unua))
               | _ -> Error (sprintf "La sintaksa vorto %s ne estas modifanto" malinflektita)
            | (Vorttipo.Pridiranto, AtributativoEsti) ->
               legiModifantojnAk (Pridiranto(malinflektita) :: listo) restantaj vortajModifantoj (Some(unua))
            | _ ->
               match lastaVorto with
               | Some(l) when l = unua -> legiModifantojnAk listo restantaj vortajModifantoj lastaVorto
               | _ -> (listo, vortoj, lastaVorto) |> Ok
         | None -> Error (sprintf "%s ne estas modifanto" unua)
      | [] -> (listo, vortoj, lastaVorto) |> Ok

   and legiObjekto (partoj: string list) vortajModifantoj (lastaVorto: string option) =
      match partoj with
      | unua :: restantaj ->
         match malinflekti unua with
         | Some(vortformoj, malinflektita) ->
            let lastaFormo = List.last vortformoj
            match lastaFormo with
            | (Vorttipo.Lokokupilo, SolaFormo) -> (Lokokupilo(malinflektita), restantaj, Some(unua)) |> Ok
            | (Vorttipo.FremdaVorto, SolaFormo) -> (FremdaVorto(malinflektita), restantaj, Some(unua)) |> Ok
            | (NombrigeblaKlaso, inflekcio) | (NenombrigeblaKlaso, inflekcio) ->
               (Objekto({ Objekto = malinflektita; Inflekcio = inflekcio }), restantaj, Some(unua)) |> Ok
            | (NenombrigeblaEco, Havaĵo) ->
               (Objekto({ Objekto = malinflektita; Inflekcio = Havaĵo}), restantaj, Some(unua)) |> Ok
            | (NenombrigeblaEco, inflekcio) | (NombrigeblaEco, inflekcio) ->
               legiObjekto restantaj vortajModifantoj (Some(unua))
               |> Result.map (fun (posedanto, restantaj, novaLastaVorto) ->
                  (Eco({ Objekto = malinflektita; Inflekcio = inflekcio }, posedanto), restantaj, novaLastaVorto))
            | _ ->
               match lastaVorto with
               | Some(l) when unua = l -> legiObjekto restantaj vortajModifantoj lastaVorto
               | _ -> Error (sprintf "%s ne havas validan inflekcion por objekto" unua)
            |> Result.bind (fun (vorto, restantaj, novaLastaVorto) ->
               legiModifantojnAk [] restantaj vortajModifantoj novaLastaVorto
               |> Result.map (fun (modifantoj, restantaj, novaLastaVorto) ->
                  if modifantoj.IsEmpty
                  then (vorto, restantaj, novaLastaVorto)
                  else (PridiritaVorto(vorto, modifantoj), restantaj, novaLastaVorto)))
         | None -> Error (sprintf "%s estas nevalida" unua)
      | [] -> Error (sprintf "Ankoraŭ bezonas vortojn por objekto, sed ne plu estas")

   let rec legiObjektojn (kvanto: int) (listo: string list) vortajModifantoj (lastaVorto: string option) =
      if kvanto = 0
      then ([], listo, lastaVorto) |> Ok
      else
         legiObjekto listo vortajModifantoj lastaVorto
         |> Result.bind
            (fun (objekto, restantaj, novaLastaVorto) ->
               legiObjektojn (kvanto - 1) restantaj vortajModifantoj novaLastaVorto
               |> Result.map
                  (fun (restantajObjektoj, restantaj, novaLastaVorto) ->
                     (objekto :: restantajObjektoj, restantaj, novaLastaVorto)))


   let legiTransitivanPredikaton 
      (valenco: int) (partoj: string list) vortajModifantoj (verbo: Verbo) (lastaVorto: string option) =
      legiObjektojn valenco partoj vortajModifantoj lastaVorto
      |> Result.bind (fun objektoj ->
         match objektoj with
         | ([ vorto1; vorto2 ], restantaj, novaLastaVorto) ->
            (Predikato2(verbo, vorto1, vorto2), restantaj, novaLastaVorto) |> Ok
         | ([ vorto1; vorto2; vorto3 ], restantaj, novaLastaVorto) ->
            (Predikato3(verbo, vorto1, vorto2, vorto3), restantaj, novaLastaVorto) |> Ok
         | _ -> Error (sprintf "%s ne havas valencon de 2 aŭ 3" verbo.Verbo))

   let legiPridirantanPredikaton (pridiranto: Verbo) (partoj: string list)
      vortajModifantoj (lastaVorto: string option) =
      legiObjektojn 1 partoj vortajModifantoj lastaVorto
      |> Result.bind (fun (objektoj, restantaj, novaLastaVorto) ->
         match objektoj with
         | [ objekto ] -> (Predikato1(pridiranto, objekto), restantaj, novaLastaVorto) |> Ok
         | _ -> Error "ne eblas legi objekton por pridiranta predikato")

   let rec legiPredikaton (listo: string list) vortajModifantoj (lastaVorto: string option) =
      match listo with
      | unua :: restantaj ->
         match malinflekti unua with
         | Some(vortformoj, malinflektita) ->
            match List.last vortformoj with
            | (TransitivaVerbo2, inflekcio) ->
               legiTransitivanPredikaton 2 restantaj vortajModifantoj
                  { Verbo = malinflektita; Inflekcio = inflekcio } (Some(unua))
            | (TransitivaVerbo3, inflekcio) ->
               legiTransitivanPredikaton 3 restantaj vortajModifantoj
                  { Verbo = malinflektita; Inflekcio = inflekcio } (Some(unua))
            | (Vorttipo.Pridiranto, PredikativoEsti) ->
               legiPridirantanPredikaton
                  { Verbo = malinflektita; Inflekcio = PredikativoEsti } restantaj
                  vortajModifantoj (Some(unua))
            | _ ->
               match lastaVorto with
               | Some(l) when unua = l ->
                  legiPredikaton restantaj vortajModifantoj lastaVorto
               | _ -> Error (sprintf "predikato ne eblas komenci per %s" unua)
         | None -> Error (sprintf "%s estas nevalida vorto" unua)
      | _ -> Error (sprintf "bezonas pli da vortoj por legi predikaton")

   let legiKondicianFrazon (listo: string list) vortajModifantoj lastaVorto =
      legiPredikaton listo vortajModifantoj lastaVorto
      |> Result.bind
         (fun (kondicio, restantaj, lastaVorto) ->
            legiPredikaton restantaj vortajModifantoj lastaVorto
            |> Result.map (fun (predikato, restantaj, novaLastaVorto) ->
               (Peral(kondicio, predikato), restantaj, novaLastaVorto)))
   
   let legiFrazon
      (partoj: string list) (lastaVorto: string option) vortajModifantoj =
      match partoj with
      | unua :: restantaj ->
         kontroli unua
         |> Option.map
            (fun vortformo ->
               match vortformo with
               | (TransitivaVerbo2, _) | (TransitivaVerbo3, _)
               | (NetransitivaVerbo1, _) | (NetransitivaVerbo2, _)
               | (Vorttipo.Pridiranto, PredikativoEsti) ->
                  legiPredikaton partoj  vortajModifantoj lastaVorto
                  |> Result.map (fun (predikato, restantaj, novaLastaVorto) ->
                      (Predikato(predikato), restantaj, novaLastaVorto))
               | (SintaksaVorto, SolaFormo) ->
                  match unua with
                  | "peral" ->
                     legiKondicianFrazon restantaj vortajModifantoj (Some("peral"))
                  | _ -> Error (sprintf "La sintaksvorto %s ne eblas komenci frazon" unua)
               | _ -> Error (sprintf "%s ne eblas komenci frazon" unua))
         |> Option.defaultValue (Error (sprintf "%s estas nevalida vorto" unua))
      | [] -> Error (sprintf "bezonas pli da vortoj")