namespace KrestiaVortilo

open Vorttipo
open Traktilaro
open Strukturo

module Legiloj =
   let rec legiModifantojnAk
      (listo: Modifanto list) (vortoj: string list) (vortajModifantoj: Set<string>) (lastaVorto: string option) =
      match vortoj with
      | unua :: restantaj ->
         match malinflektiUnuFoje unua with
         | Some(malinflektita, _, originalaFormo) ->
            match originalaFormo with
            | (MalantaŭModifanto, SolaFormo) ->
               match malinflektita with
               | "vol" ->
                  legiVorton restantaj vortajModifantoj (Some("vol"))
                  |> Result.bind
                     (fun (volObjekto, restantaj, lastaVorto) ->
                        legiModifantojnAk (Modifanto1(malinflektita, volObjekto) :: listo)
                           restantaj vortajModifantoj lastaVorto)
               | _ when vortajModifantoj.Contains(malinflektita) ->
                  legiModifantojnAk (Modifanto0(malinflektita) :: listo) restantaj vortajModifantoj (Some(unua))
               | _ -> Error (sprintf "La sintaksa vorto %s ne estas modifanto" malinflektita)
            | (Vorttipo.Pridiranto, Inflekcio.AtributivoEstiMalantaŭ) ->
               legiModifantojnAk (Pridiranto(malinflektita) :: listo) restantaj vortajModifantoj (Some(unua))
            | _ ->
               match lastaVorto with
               | Some(l) when l = unua -> legiModifantojnAk listo restantaj vortajModifantoj lastaVorto
               | _ -> (listo, vortoj, lastaVorto) |> Ok
         | None -> Error (sprintf "%s ne estas modifanto" unua)
      | [] -> (listo, vortoj, lastaVorto) |> Ok

   and legiInflektitaVortonAk (inflekcioj: InflekcioŜtupo list) (partoj: string list) vortajModifantoj (lastaVorto: string option) =
      match partoj with
      | unua :: restantaj ->
         malinflektiUnuFoje unua
         |> Option.map (fun (malinflektita, formo, (_, originalaInflekcio)) ->
            let novaInflekcioListo =
               match originalaInflekcio with
               | Infinitivo -> (inflekcioj, restantaj, lastaVorto) |> Ok
               | SolaFormo -> (inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.Difinito -> (NekonitaNombro :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.UnuNombro -> (UnuNombro :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.Havaĵo -> (Havaĵo :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.PluraNombro -> (PluraNombro :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.Progresivo -> (Progresivo :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.Perfekto -> (Perfekto :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.Estonteco -> (Estonteco :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.NominativoVolo -> (NominativoVolo :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.AkuzativoVolo -> (AkuzativoVolo :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.DativoVolo -> (DativoVolo :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.PredikativoEsti -> (PredikativoEsti :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.AtributivoEstiMalantaŭ -> (AtributativoEsti :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.Havado -> (PredikativoHavi :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.AtributativoHavi -> (AtributativoHavi :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.Imperativo -> (Imperativo :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.Patiento -> (Patiento :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.Aganto -> (Aganto :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.Translativo -> (Translativo :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.Ĝerundo -> (InflekcioŜtupo.Ĝerundo :: inflekcioj, restantaj, lastaVorto) |> Ok
               | Inflekcio.PartaNominativo
               | Inflekcio.PartaAkuzativo
               | Inflekcio.PartaDativo ->
                  legiVorton restantaj vortajModifantoj (Some(unua))
                  |> Result.map
                     (fun (objekto, restantaj, novaLastaVorto) ->
                        let partaŜtupo =
                           match originalaInflekcio with
                           | Inflekcio.PartaNominativo -> PartaNominativo(objekto)
                           | Inflekcio.PartaAkuzativo -> PartaAkuzativo(objekto)
                           | Inflekcio.PartaDativo -> PartaDativo(objekto)
                           | _ -> failwith "neebla"
                        (partaŜtupo :: inflekcioj, restantaj, novaLastaVorto))
               | _ -> failwith (sprintf "TODO: ne eblas trakti %A" originalaInflekcio)
            novaInflekcioListo
            |> Result.bind (fun (novaListo, restantaj, novaLastaVorto) ->
               match formo with
                  | (Vorttipo.Lokokupilo, SolaFormo) ->
                     (Lokokupilo(malinflektita), restantaj, lastaVorto) |> Ok
                  | (tipo, Infinitivo) ->
                     match tipo with
                     | NombrigeblaKlaso
                     | NenombrigeblaKlaso ->
                        (BazaVorto({ Vorto = malinflektita; Inflekcio = novaListo }),
                           restantaj, Some(unua)) |> Ok
                     | TransitivaVerbo | DutransitivaVerbo
                     | NetransitivaVerbo | NedirektaTransitivaVerbo
                     | OblikaNetransitivaVerbo | OblikaTransitivaVerbo
                     | NedirektaNetransitivaVerbo | MalplenaVerbo
                     | Vorttipo.Pridiranto ->
                        (BazaVorto({ Vorto = malinflektita; Inflekcio = novaListo }), restantaj, Some(unua)) |> Ok
                     | AntaŭNombrigeblaEco | AntaŭNenombrigeblaEco ->
                        match originalaInflekcio with
                        | Inflekcio.Havaĵo ->
                           (BazaVorto({ Vorto = malinflektita; Inflekcio = novaListo }), restantaj, novaLastaVorto) |> Ok
                        | _ ->
                           legiVorton restantaj vortajModifantoj (Some(unua))
                           |> Result.map (fun (posedanto, restantaj, novaLastaVorto) ->
                              (Eco({ Vorto = malinflektita; Inflekcio = novaListo },
                                 posedanto), restantaj, novaLastaVorto))
                     | _ -> Error (sprintf "ne eblas trakti la infinitivon %s" malinflektita)
                  | (_, Inflekcio.Translativo)
                  | (_, Inflekcio.PartaNominativo)
                  | (_, Inflekcio.PartaAkuzativo)
                  | (_, Inflekcio.PartaDativo) ->
                     legiInflektitaVortonAk novaListo (malinflektita :: restantaj) vortajModifantoj novaLastaVorto
                  | (Vorttipo.FremdaVorto, SolaFormo) ->
                     (FremdaVorto(malinflektita), restantaj, Some(unua)) |> Ok
                  | (_, Inflekcio.Ĝerundo) ->
                     legiInflektitaVortonAk (InflekcioŜtupo.Ĝerundo :: inflekcioj) restantaj vortajModifantoj lastaVorto
                  | _ ->
                     match lastaVorto with
                     | Some(l) when l = unua -> legiInflektitaVortonAk inflekcioj restantaj vortajModifantoj lastaVorto
                     | _ -> Error (sprintf "%s ne havas trakteblan inflekcion" unua)))
         |> Option.defaultValue (Error (sprintf "ne eblas malinflekti %s" unua))
      | [] -> Error "bezonas pli da vortoj por legi ĉiujn inflekciojn"

   and legiVorton (partoj: string list) vortajModifantoj (lastaVorto: string option) =
      legiInflektitaVortonAk [] partoj vortajModifantoj lastaVorto
      |> Result.bind (fun (vorto, restantaj, lastaVorto) ->
         legiModifantojnAk [] restantaj vortajModifantoj lastaVorto
         |> Result.map (fun (modifantoj, restantaj, lastaVorto) ->
            if modifantoj.IsEmpty
            then (vorto, restantaj, lastaVorto)
            else (PridiritaVorto(vorto, modifantoj), restantaj, lastaVorto)))

   let rec legiObjektojn (kvanto: int) (listo: string list) vortajModifantoj (lastaVorto: string option) =
      if kvanto = 0
      then ([], listo, lastaVorto) |> Ok
      else
         legiVorton listo vortajModifantoj lastaVorto
         |> Result.bind
            (fun (objekto, restantaj, novaLastaVorto) ->
               legiObjektojn (kvanto - 1) restantaj vortajModifantoj novaLastaVorto
               |> Result.map
                  (fun (restantajObjektoj, restantaj, novaLastaVorto) ->
                     (objekto :: restantajObjektoj, restantaj, novaLastaVorto)))

   let legiTransitivanPredikaton 
      (valenco: int) (partoj: string list) vortajModifantoj (verbo: BazaVorto) (lastaVorto: string option) =
      legiObjektojn valenco partoj vortajModifantoj lastaVorto
      |> Result.bind (fun objektoj ->
         match objektoj with
         | ([ vorto1; vorto2 ], restantaj, novaLastaVorto) ->
            (Predikato2(verbo, vorto1, vorto2), restantaj, novaLastaVorto) |> Ok
         | ([ vorto1; vorto2; vorto3 ], restantaj, novaLastaVorto) ->
            (Predikato3(verbo, vorto1, vorto2, vorto3), restantaj, novaLastaVorto) |> Ok
         | _ -> Error (sprintf "%s ne havas valencon de 2 aŭ 3" verbo.Vorto))

   let legiPridirantanPredikaton (pridiranto: BazaVorto) (partoj: string list)
      vortajModifantoj (lastaVorto: string option) =
      legiObjektojn 1 partoj vortajModifantoj lastaVorto
      |> Result.bind (fun (objektoj, restantaj, novaLastaVorto) ->
         match objektoj with
         | [ objekto ] -> (Predikato1(pridiranto, objekto), restantaj, novaLastaVorto) |> Ok
         | _ -> Error "ne eblas legi objekton por pridiranta predikato")

   let rec legiPredikaton (partoj: string list) vortajModifantoj (lastaVorto: string option) =
      legiVorton partoj vortajModifantoj lastaVorto
      |> Result.bind (fun (vorto, restantaj, lastaVorto) ->
         match vorto with
         | BazaVorto(verbo) ->
            match kontroli verbo.Vorto with
            | Some((vorttipo, _)) ->
               match vorttipo with
               | TransitivaVerbo -> legiTransitivanPredikaton 2 restantaj vortajModifantoj verbo lastaVorto
               | DutransitivaVerbo -> legiTransitivanPredikaton 3 restantaj vortajModifantoj verbo lastaVorto
               | Vorttipo.Pridiranto when verbo.Inflekcio = [ PredikativoEsti ] ->
                  legiPridirantanPredikaton verbo restantaj vortajModifantoj lastaVorto
               | _ -> Error (sprintf "ne eblas trakti kiel verbo: %A" vorto)
            | None -> failwith "???"
         | _ -> Error (sprintf "bezonas verbon, sed legis: %A" vorto))

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
               | (TransitivaVerbo, _) | (DutransitivaVerbo, _)
               | (NetransitivaVerbo, _) | (NedirektaTransitivaVerbo, _)
               | (Vorttipo.Pridiranto, Inflekcio.PredikativoEsti) ->
                  legiPredikaton partoj vortajModifantoj lastaVorto
                  |> Result.map (fun (predikato, restantaj, novaLastaVorto) ->
                      (Predikato(predikato), restantaj, novaLastaVorto))
               | (MalantaŭModifanto, SolaFormo) ->
                  match unua with
                  | "peral" ->
                     legiKondicianFrazon restantaj vortajModifantoj (Some("peral"))
                  | _ -> Error (sprintf "La sintaksvorto %s ne eblas komenci frazon" unua)
               | _ -> Error (sprintf "%s ne eblas komenci frazon" unua))
         |> Option.defaultValue (Error (sprintf "%s estas nevalida vorto" unua))
      | [] -> Error (sprintf "bezonas pli da vortoj")