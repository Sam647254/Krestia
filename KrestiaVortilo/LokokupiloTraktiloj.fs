namespace KrestiaVortilo

open Vorttipo

module LokokupiloTraktiloj =
   let trakiloj : Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if vorto.StartsWith("h") || vorto.StartsWith("w")
           then Some (Lokokupilo, SolaFormo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (Lokokupilo, SolaFormo)) }
      ]