namespace KrestiaVortilo

open Vorttipo

module PridirantoTraktiloj =
   let traktiloj: Vorttraktilo list = [
      { Formo = (Pridiranto, Infinitivo)
        Kontroli = fun vorto -> vorto.EndsWith("d")
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto -> (vorto, (Pridiranto, Infinitivo)) }

      { Formo = (Pridiranto, NekonitaNombro)
        Kontroli = fun vorto -> vorto.EndsWith("de")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 1), (Pridiranto, Infinitivo)) }

      { Formo = (Pridiranto, UnuNombro)
        Kontroli = fun vorto -> vorto.EndsWith("da")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 1), (Pridiranto, Infinitivo)) }

      { Formo = (Pridiranto, PluraNombro)
        Kontroli = fun vorto -> vorto.EndsWith("die")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (Pridiranto, Infinitivo)) }

      { Formo = (Pridiranto, Havaĵo)
        Kontroli = fun vorto -> vorto.EndsWith("du")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 1), (Pridiranto, Infinitivo)) }

      { Formo = (Pridiranto, Progresivo)
        Kontroli = fun vorto -> vorto.EndsWith("dia")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (Pridiranto, Infinitivo)) }

      { Formo = (Pridiranto, Perfekto)
        Kontroli = fun vorto -> vorto.EndsWith("do")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 1), (Pridiranto, Infinitivo)) }

      { Formo = (Pridiranto, AtributativoEsti)
        Kontroli = fun vorto -> vorto.EndsWith("dea")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (Pridiranto, Infinitivo)) }

      { Formo = (Pridiranto, Translativo)
        Kontroli = fun vorto -> vorto.EndsWith("dis")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (Pridiranto, Infinitivo)) }

      { Formo = (Pridiranto, Ĝerundo)
        Kontroli = fun vorto -> vorto.EndsWith("di")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 1), (Pridiranto, Infinitivo)) }

      { Formo = (Pridiranto, Igo)
        Kontroli = fun vorto -> vorto.EndsWith("det")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (Pridiranto, Infinitivo)) }
      ]