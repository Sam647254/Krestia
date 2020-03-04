namespace KrestiaVortilo

open Vorttipo

module PartaNetransitivaVerboTraktiloj =
   let normaligi (infinitivo: string) =
      match infinitivo with
      | _ when infinitivo.EndsWith("en") -> (NetransitivaVerbo, PartaNominativo)
      | _ -> (NedirektaNetransitivaVerbo, Infinitivo)

   let traktiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if vorto.EndsWith("n")
           then Some (NedirektaNetransitivaVerbo, Infinitivo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (NedirektaNetransitivaVerbo, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("nia")
           then Some (NedirektaNetransitivaVerbo, Progresivo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 2)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("nio")
           then Some (NedirektaNetransitivaVerbo, Perfekto)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 2)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("nela")
           then Some (NedirektaNetransitivaVerbo, Estonteco)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 3)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("neri")
           then Some (NedirektaNetransitivaVerbo, DativoVolo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 3)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("nema")
           then Some (NedirektaNetransitivaVerbo, Ĝerundo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 3)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("nom")
           then Some (NedirektaNetransitivaVerbo, PartaDativo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 2)
           (malinflektita, normaligi malinflektita) }
   ]