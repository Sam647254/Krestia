namespace KrestiaVortilo

open Vorttipo

module Traktilaro =
   let traktilaro =
      LokokupiloTraktiloj.trakiloj @
      NombrigeblaKlasoTraktiloj.trakiloj @
      NenombrigeblaKlasoTraktiloj.trakiloj @
      EcoTraktiloj.traktiloj @
      TransitivaVerboTraktiloj.trakiloj @
      NetransitivaVerboTraktiloj.trakiloj @
      PridirantoTraktiloj.traktiloj @
      FremdaVortoTraktiloj.traktiloj @
      SpecialajTraktiloj.traktiloj

   let kontroli (vorto: string) =
      traktilaro
      |> List.tryPick (fun trakilo -> trakilo.Kontroli vorto)

   let malinflekti (inflektitaVorto: string) =
      let rec malinflektiAk (inflektitaVorto: string) (listo: Vortformo list) =
         traktilaro
         |> List.tryFind (fun traktilo -> traktilo.Kontroli inflektitaVorto |> Option.isSome)
         |> Option.bind (fun traktilo ->
            let (malinflektitaVorto, malinflektitaFormo) = inflektitaVorto |> traktilo.Malinflekti
            if malinflektitaVorto = inflektitaVorto
            then Some (listo, malinflektitaVorto)
            else malinflektitaFormo :: listo |> malinflektiAk malinflektitaVorto)
      inflektitaVorto
      |> kontroli
      |> Option.bind (fun komencaFormo -> malinflektiAk inflektitaVorto [ komencaFormo ])