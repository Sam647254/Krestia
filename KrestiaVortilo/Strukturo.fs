namespace KrestiaVortilo

open Vorttipo

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

   let strukturigi (eniro: string) (ĉuEkzistas: string -> bool) (valenco: string -> int option):
      Result<Frazo, string> =
      let vortoj = eniro.Split(' ') |> List.ofArray
      Error "???"