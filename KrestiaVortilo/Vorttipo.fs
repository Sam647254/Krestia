namespace KrestiaVortilo

module Vorttipo =
   type Inflekcio =
   | Difinito // D
   | Havaĵo // H
   | Fokuso // F
   | Progresivo // P
   | Perfekto // p
   | Intenco // I
   | Desiderativo // d
   | PredikativoEsti // E
   | AtributivoEstiAntaŭ // A
   | AtributivoEstiMalantaŭ // a
   | Havado // h
   | Imperativo // i
   | Argumento1 // 1
   | Argumento2 // 2
   | Argumento3 // 3
   | Ekzistado // e
   | Hortativo // t
   | Translativo // T
   | Ĝerundo // Ĝ
   | SpecifaĜerundo // ĝ
   | PartaUjo1 // U
   | PartaUjo2 // J
   | PartaUjo3 // O
   | SolaFormo // S
   | Sola // sK
   | Reflekcio // R
   | UnueUjo2 // 4
   | UnueUjo3 // 5
   | Optativo // o
   | Kvalito // K
   | Hipoteza // n
   | Apartigita // X
   | FremdaVorto // @
   | Cifero // #
   | Predikato // &

   type Vorttipo =
   | NombrigeblaKlaso // K
   | NenombrigeblaKlaso // k
   | MalantaŭRekordo // L
   | AntaŭRekordo // l
   | MalantaŭNombrigeblaEco // E
   | AntaŭNombrigeblaEco // e
   | MalantaŭNenombrigeblaEco // P
   | AntaŭNenombrigeblaEco // p
   | TransitivaVerbo // T
   | DutransitivaVerbo // D
   | NetransitivaVerbo // t
   | NedirektaTransitivaVerbo // N
   | MalplenaVerbo // M
   | OblikaNetransitivaVerbo // n
   | OblikaTransitivaVerbo // O
   | NedirektaNetransitivaVerbo // Y
   | Pridiranto // D
   | Lokokupilo // Q
   | MalantaŭModifanto // <
   | AntaŭModifanto // >
   | Makro // !
   | FremdaVorto // F
   | Cifero // C

   type Vortformo = Vorttipo * Inflekcio
   type VorttipoKontrolilo = (string -> bool)
   type Vorttraktilo = {
      Kontroli : (string -> Vortformo option)
      Inflekti : (Vortformo -> string -> string option)
      Malinflekti : (string -> (string * Vortformo))
   }

   let neinflektebla = fun _formo _vorto -> None
   let ĉuHavasFinaĵon finaĵoj (vorto: string) = finaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))