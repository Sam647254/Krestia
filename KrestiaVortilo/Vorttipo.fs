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
   | AtributativoEstiAntaŭ
   | AtributativoEstiMalantaŭ
   | Havado
   | UnuHavado
   | PluraHavado
   | AtributativoHavi
   | Imperativo
   | Patiento
   | Aganto
   | Ekzistado
   | UnuEkzistado
   | PluraEkzistado
   | Invito
   | Translativo
   | Ĝerundo
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

   type Vorttipo =
   | NombrigeblaKlaso
   | NenombrigeblaKlaso
   | Rekordo
   | NombrigeblaEco
   | NenombrigeblaEco
   | TransitivaVerbo2
   | TransitivaVerbo3
   | NetransitivaVerbo1
   | NetransitivaVerbo2
   | MalplenaVerbo
   | PartaTransitivaVerbo1
   | PartaTransitivaVerbo2
   | PartaNetransitivaVerbo
   | Pridiranto
   | Lokokupilo
   | SintaksaVorto
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