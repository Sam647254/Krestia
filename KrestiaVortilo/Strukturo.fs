namespace KrestiaVortilo

open Vorttipo
open Traktilaro

module Strukturo =
   type Verbo = {
      Verbo : string
      Inflekcio : Inflekcio
   }

   type Objekto = {
      Objekto : string
      Inflekcio : Inflekcio
   }

   type Modifanto =
   | Modifanto0 of string
   | Modifanto1 of Modifanto : string * Vorto : string

   type Vorto =
   | Lokokupilo of string
   | Objekto of Objekto
   | PridiritaVorto of Vorto * Modifantoj : Modifanto list
   | FremdaVorto of string

   type Frazo =
   | Predikato0 of Verbo : Verbo
   | Predikato1 of Verbo : Verbo * Vorto1 : Vorto
   | Predikato2 of Verbo : Verbo * Vorto1 : Vorto * Vorto2 : Vorto
   | Predikato3 of Verbo : Verbo * Vorto1 : Vorto * Vorto2 : Vorto * Vorto3 : Vorto

   let strukturigi (eniro: string) (valencoDe: string -> int option) (vortajModifantoj: Set<string>)
      : Result<Frazo, string> =
      let partoj = eniro.Split(' ') |> List.ofArray

      let rec legiModifantojnAk (listo: Modifanto list) (vortoj: string list) =
         match vortoj with
         | unua :: restantaj ->
            match malinflekti unua with
            | Some(vortformoj, malinflektita) ->
               let lastaFormo = List.last vortformoj
               match lastaFormo with
               | (SintaksaVorto, SolaFormo) ->
                  if vortajModifantoj.Contains(malinflektita)
                  then legiModifantojnAk (Modifanto0(malinflektita) :: listo) restantaj
                  else Error (sprintf "La sintaksa vorto %s ne estas modifanto" malinflektita)
               | (Pridiranto, AtributativoEsti) ->
                  legiModifantojnAk (Modifanto0(malinflektita) :: listo) restantaj
               | _ -> (listo, vortoj) |> Ok
            | None -> Error (sprintf "%s ne estas modifanto" unua)
         | [] -> (listo, vortoj) |> Ok

      let rec legiObjektojn (kvanto: int) (listo: string list) =
         if kvanto = 0
         then [] |> Ok
         else
            match listo with
            | unua :: restantaj ->
               match malinflekti unua with
               | Some(vortformoj, malinflektita) ->
                  let lastaFormo = List.last vortformoj
                  match lastaFormo with
                  | (Vorttipo.Lokokupilo, SolaFormo) -> Lokokupilo(malinflektita) |> Ok
                  | (Vorttipo.FremdaVorto, SolaFormo) -> FremdaVorto(malinflektita) |> Ok
                  | (NombrigeblaKlaso, inflekcio) ->
                     Objekto({ Objekto = malinflektita; Inflekcio = inflekcio }) |> Ok
                  | _ -> Error (sprintf "%s ne havas validan inflekcion" unua)
                  |> Result.bind (fun vorto ->
                     legiModifantojnAk [] restantaj
                     |> Result.map (fun (modifantoj, restantaj) ->
                        if modifantoj.IsEmpty
                        then (vorto, restantaj)
                        else (PridiritaVorto(vorto, modifantoj), restantaj)))
               | None -> Error (sprintf "%s estas nevalida" unua)
            | [] -> Error "Ankoraŭ bezonas vortojn, sed ne plu estas"
            |> Result.bind (fun (sekva, restantaj) ->
               legiObjektojn (kvanto - 1) restantaj
               |> Result.map (fun restantajObjektoj -> sekva :: restantajObjektoj))

      let strukturigiAk (listo: string list) =
         match listo with
         | unua :: restantaj ->
            let formo = malinflekti unua
            match formo with
            | Some(vortoformoj, malinflektita) ->
               let lastaFormo = List.last vortoformoj
               match lastaFormo with
               | (TransitivaVerbo, inflekcio) ->
                  valencoDe malinflektita
                  |> Option.map (fun valenco ->
                     let verbo = { Verbo = malinflektita; Inflekcio = inflekcio }
                     legiObjektojn valenco restantaj
                     |> Result.bind (fun objektoj ->
                        match objektoj with
                        | [ vorto1; vorto2 ] ->
                           Predikato2(verbo, vorto1, vorto2) |> Ok
                        | [ vorto1; vorto2; vorto3 ] ->
                           Predikato3(verbo, vorto1, vorto2, vorto3) |> Ok
                        | _ -> Error (sprintf "%s havas nevalidan valencon" malinflektita)))
                  |> Option.defaultValue (Error (sprintf "%s ne havas valencon" malinflektita))
               | _ -> Error "???"
            | None -> Error (sprintf "%s estas nevalida" unua)
         | [] -> Error "???"
      strukturigiAk partoj