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
   | Vorto of Objekto
   | PridiritaVorto of Vorto : Objekto * Modifantoj : Modifanto list

   type Frazo =
   | Predikato0 of Verbo : Verbo
   | Predikato1 of Verbo : Verbo * Vorto1 : Vorto
   | Predikato2 of Verbo : Verbo * Vorto1 : Vorto * Vorto2 : Vorto
   | Predikato3 of Verbo : Verbo * Vorto1 : Vorto * Vorto2 : Vorto * Vorto3 : Vorto

   let strukturigi (eniro: string) (valencoDe: string -> int option): Result<Frazo, string> =
      let partoj = eniro.Split(' ') |> List.ofArray

      let rec legiObjektoj (kvanto: int) (listo: string list) =
         if kvanto = 0
         then [] |> Ok
         else
            let sekva, restantaj =
               match listo with
               | unua :: restantaj ->
                  let sekvaObjekto =
                     match malinflekti unua with
                     | Some(vortformoj, malinflektita) ->
                        let lastaFormo = List.last vortformoj
                        match lastaFormo with
                        | (Vorttipo.Lokokupilo, SolaFormo) -> Lokokupilo(malinflektita) |> Ok
                        | _ -> Error (sprintf "%s ne havas validan inflekcion" unua)
                     | None -> Error (sprintf "%s estas nevalida" unua)
                  (sekvaObjekto, restantaj)
               | [] -> (Error ("Ankoraŭ bezonas vortojn, sed ne plu estas"), [])
            match sekva with
            | Ok(x) ->
               legiObjektoj (kvanto - 1) restantaj
               |> Result.map (fun restantajObjektoj -> x :: restantajObjektoj)
            | Error(x) -> Error(x)

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
                     legiObjektoj valenco restantaj
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