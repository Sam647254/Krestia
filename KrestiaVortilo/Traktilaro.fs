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
      PridirantoTraktiloj.traktiloj

   let kontroli (vorto: string) =
      traktilaro
      |> List.tryFind (fun trakilo -> trakilo.Kontroli vorto)
      |> Option.map (fun trakilo -> trakilo.Formo)

   let malinflekti (inflektitaVorto: string) =
      let rec malinflektiAk (inflektitaVorto: string) (listo: Vortformo list) =
         traktilaro
         |> List.tryFind (fun traktilo -> traktilo.Kontroli inflektitaVorto)
         |> Option.bind (fun traktilo ->
            let (malinflektitaVorto, malinflektitaFormo) = inflektitaVorto |> traktilo.Malinflekti
            if malinflektitaVorto = inflektitaVorto
            then Some (listo, malinflektitaVorto)
            else malinflektitaFormo :: listo |> malinflektiAk malinflektitaVorto)
      inflektitaVorto
      |> kontroli
      |> Option.bind (fun komencaFormo -> malinflektiAk inflektitaVorto [ komencaFormo ])