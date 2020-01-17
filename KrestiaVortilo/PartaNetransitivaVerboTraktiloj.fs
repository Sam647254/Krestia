namespace KrestiaVortilo

open Vorttipo

module PartaNetransitivaVerboTraktiloj =
   let normaligi (infinitivo: string) =
      match infinitivo with
      | _ when infinitivo.EndsWith("en") -> (NetransitivaVerbo1, PartaNominativo)
      | _ -> (PartaNetransitivaVerbo, Infinitivo)

   let traktiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if vorto.EndsWith("n")
           then Some (PartaNetransitivaVerbo, Infinitivo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (PartaNetransitivaVerbo, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("nia")
           then Some (PartaNetransitivaVerbo, Progresivo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 2)
           (malinflektita, normaligi malinflektita) }
   ]