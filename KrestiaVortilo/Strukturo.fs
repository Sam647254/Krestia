namespace KrestiaVortilo

open Vorttipo
open Traktilaro

module Strukturo =
   type BazaVorto = {
      Vorto : string
      Inflekcio : InflekcioŜtupo list
   }

   and Modifanto =
   | Pridiranto of string
   | Modifanto0 of string
   | Modifanto1 of Modifanto : string * Vorto : Vorto

   and InflekcioŜtupo =
   | NekonitaNombro
   | UnuNombro
   | Havaĵo
   | PluraNombro
   | Progresivo
   | Perfekto
   | Estonteco
   | NominativoVolo
   | AkuzativoVolo
   | DativoVolo
   | PredikativoEsti
   | AtributativoEsti
   | PredikativoHavi
   | AtributativoHavi
   | Imperativo
   | Patiento
   | Aganto
   | Translativo
   | Ĝerundo
   | PartaNominativo of Vorto
   | PartaAkuzativo of Vorto
   | PartaDativo of Vorto
   | Pasivigo of Vorto
   | Igo

   and Vorto =
   | Lokokupilo of string
   | BazaVorto of BazaVorto
   | Eco of BazaVorto * Vorto
   | PridiritaVorto of Vorto * Modifantoj : Modifanto list
   | FremdaVorto of string

   and Predikato =
   | Predikato0 of Verbo : BazaVorto
   | Predikato1 of Verbo : BazaVorto * Vorto1 : Vorto
   | Predikato2 of Verbo : BazaVorto * Vorto1 : Vorto * Vorto2 : Vorto
   | Predikato3 of Verbo : BazaVorto * Vorto1 : Vorto * Vorto2 : Vorto * Vorto3 : Vorto

   type Frazo =
   | Predikato of Predikato
   | Peral of Predikato * Predikato