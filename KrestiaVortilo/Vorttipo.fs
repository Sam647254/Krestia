﻿namespace KrestiaVortilo

module Vorttipo =
   type Inflekcio =
   | Infinitivo
   | Difinito
   | UnuNombro
   | Havaĵo
   | UnuHavaĵo
   | PluraHavaĵo
   | PluraNombro
   | Progresivo
   | Perfekto
   | Estonteco
   | NominativoVolo
   | AkuzativoVolo
   | DativoVolo
   | PredikativoEsti
   | AtributivoEstiAntaŭ
   | AtributivoEstiMalantaŭ
   | Havado
   | UnuHavado
   | PluraHavado
   | AtributativoHavi
   | Imperativo
   | Patiento
   | NedirektaPatiento
   | Aganto
   | Ekzistado
   | UnuEkzistado
   | PluraEkzistado
   | Invito
   | Translativo
   | Ĝerundo
   | SpecifaĜerundo
   | PartaNominativo
   | PartaAkuzativo
   | PartaDativo
   | Pasivigo
   | Igo
   | SolaFormo
   | Egigo
   | Etigo
   | Sola
   | UnuSola
   | PluraSola
   | Reflekcio
   | Okazo
   | AktualaOkazo
   | FinitaOkazo

   type Vorttipo =
   | NombrigeblaKlaso
   | NenombrigeblaKlaso
   | MalantaŭRekordo
   | AntaŭRekordo
   | MalantaŭNombrigeblaEco
   | AntaŭNombrigeblaEco
   | MalantaŭNenombrieblaEco
   | AntaŭNenombrigeblaEco
   | TransitivaVerbo
   | DutransitivaVerbo
   | NetransitivaVerbo
   | NedirektaTransitivaVerbo
   | MalplenaVerbo
   | OblikaNetransitivaVerbo
   | OblikaTransitivaVerbo
   | NedirektaNetransitivaVerbo
   | Pridiranto
   | Lokokupilo
   | MalantaŭModifanto
   | AntaŭModifanto
   | Makro
   | FremdaVorto

   type Vortformo = Vorttipo * Inflekcio
   type VorttipoKontrolilo = (string -> bool)
   type Vorttraktilo = {
      Kontroli : (string -> Vortformo option)
      Inflekti : (Vortformo -> string -> string option)
      Malinflekti : (string -> (string * Vortformo))
   }

   let neinflektebla = fun _formo _vorto -> None
   let ĉuHavasFinaĵon finaĵoj (vorto: string) = finaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))