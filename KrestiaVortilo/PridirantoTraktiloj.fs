namespace KrestiaVortilo

open Vorttipo

module PridirantoTraktiloj =
   let normaligi (infinitivo: string) =
      match infinitivo with
      | _ when infinitivo.EndsWith("rid") -> (Pridiranto, Egigo)
      | _ when infinitivo.EndsWith("rod") -> (Pridiranto, Etigo)
      | _ -> (Pridiranto, Infinitivo)

   let traktiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if vorto.EndsWith("d") && (not(vorto.EndsWith("drid") || vorto.EndsWith("drod")))
           then Some (Pridiranto, Infinitivo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (Pridiranto, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("de")
           then Some (Pridiranto, Difinito)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 1)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("da")
           then Some (Pridiranto, UnuNombro)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 1)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("die")
           then Some (Pridiranto, PluraNombro)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 2)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("du")
           then Some (Pridiranto, Havaĵo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 1)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dia")
           then Some (Pridiranto, PredikativoEsti)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 2)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("do")
           then Some (Pridiranto, Perfekto)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 1)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dea")
           then Some (Pridiranto, AtributativoEsti)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 2)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dis")
           then Some (Pridiranto, Translativo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 2)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("di")
           then Some (Pridiranto, Ĝerundo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 1)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("det")
           then Some (Pridiranto, Igo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 2)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("drid")
           then Some (Pridiranto, Egigo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 3)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("drod")
           then Some (Pridiranto, Etigo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 3)
           (malinflektita, normaligi malinflektita) }
      ]