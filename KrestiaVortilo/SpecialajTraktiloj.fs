namespace KrestiaVortilo

open Vorttipo

module SpecialajTraktiloj =
   let traktiloj: Vorttraktilo list = [
      { Formo = (SintaksaVorto, SolaFormo)
        Kontroli = fun vorto -> vorto.EndsWith("l") || vorto.EndsWith("r")
        Inflekti = neinflektebla
        Malinflekti = (fun vorto -> (vorto, (SintaksaVorto, SolaFormo))) }
      ]