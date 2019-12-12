namespace KrestiaVortilo

open Vorttipo

module LokokupiloTraktiloj =
   let trakiloj : Vorttraktilo list = [
      { Formo = (Lokokupilo, SolaFormo)
        Kontroli = (fun vorto -> vorto.StartsWith("h") || vorto.StartsWith("w"))
        Inflekti = neinflektebla
        Malinflekti = (fun vorto -> (vorto, (Lokokupilo, SolaFormo))) }
      ]