namespace KrestiaVortilo

open Vorttipo

module Traktilaro =
   let traktilaro =
      FremdaVortoTraktiloj.traktiloj @
      LokokupiloTraktiloj.trakiloj @
      NombrigeblaKlasoTraktiloj.trakiloj @
      NenombrigeblaKlasoTraktiloj.trakiloj @
      EcoTraktiloj.traktiloj @
      TransitivaVerboTraktiloj.trakiloj @
      NetransitivaVerboTraktiloj.trakiloj @
      PartaNetransitivaVerboTraktiloj.traktiloj @
      MalplenaVerboTraktiloj.traktiloj @
      PridirantoTraktiloj.traktiloj @
      SpecialajTraktiloj.traktiloj

   let kontroli (vorto: string) =
      traktilaro
      |> List.tryPick (fun trakilo -> trakilo.Kontroli vorto)

   let malinflektiUnuFoje (inflektitaVorto: string) =
      traktilaro
      |> List.tryFind (fun traktilo -> traktilo.Kontroli inflektitaVorto |> Option.isSome)
      |> Option.map (fun traktilo ->
         traktilo.Malinflekti inflektitaVorto
         |> fun (malinflektita, novaFormo) ->
            (malinflektita, novaFormo, traktilo.Kontroli inflektitaVorto
            |> Option.defaultWith (fun () -> failwith "neebla")))