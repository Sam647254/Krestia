namespace KrestiaVortilo

open Vorttipo

module PridirantoTraktiloj =
   let traktiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if vorto.EndsWith("d")
           then Some (Pridiranto, Infinitivo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (Pridiranto, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("de")
           then Some (Pridiranto, NekonitaNombro)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 1), (Pridiranto, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("da")
           then Some (Pridiranto, UnuNombro)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 1), (Pridiranto, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("die")
           then Some (Pridiranto, PluraNombro)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (Pridiranto, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("du")
           then Some (Pridiranto, Havaĵo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 1), (Pridiranto, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dia")
           then Some (Pridiranto, PredikativoEsti)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (Pridiranto, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("do")
           then Some (Pridiranto, Perfekto)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 1), (Pridiranto, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dea")
           then Some (Pridiranto, AtributativoEsti)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (Pridiranto, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dis")
           then Some (Pridiranto, Translativo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (Pridiranto, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("di")
           then Some (Pridiranto, Ĝerundo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 1), (Pridiranto, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("det")
           then Some (Pridiranto, Igo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (Pridiranto, Infinitivo)) }
      ]