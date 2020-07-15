namespace KrestiaVortilo

module Vorttipo =
   type Inflekcio =
   | Infinitivo
   | Difinito
   | UnuNombro
   | Havaĵo
   | UnuHavaĵo
   | PluraHavaĵo
   | PluraNombro
   | Fokuso
   | UnuFokuso
   | PluraFokuso
   | Progresivo
   | Perfekto
   | Estonteco
   | Ujo1Volo
   | Ujo2Volo
   | Ujo3Volo
   | PredikativoEsti
   | AtributivoEstiAntaŭ
   | AtributivoEstiMalantaŭ
   | Havado
   | UnuHavado
   | PluraHavado
   | AtributativoHavi
   | Imperativo
   | Argumento1
   | Argumento2
   | Argumento3
   | Ekzistado
   | UnuEkzistado
   | PluraEkzistado
   | Invito
   | Translativo
   | Ĝerundo
   | SpecifaĜerundo
   | PartaUjo1
   | PartaUjo2
   | PartaUjo3
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
   | UnueUjo2
   | UnueUjo3
   | Subklaŭzo

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