namespace KrestiaVortilo

open Vorttipo

module SpecialajTraktiloj =
   let traktiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if vorto.EndsWith("l") || vorto.EndsWith("r")
           then Some (SintaksaVorto, SolaFormo)
           else None
        Inflekti = neinflektebla
        Malinflekti = (fun vorto -> (vorto, (SintaksaVorto, SolaFormo))) }
      ]