namespace KrestiaVortilo

open Vorttipo

module SpecialajTraktiloj =
   let traktiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if vorto.EndsWith("l") || vorto.EndsWith("r")
           then Some (MalantaŭModifanto, SolaFormo)
           else None
        Inflekti = neinflektebla
        Malinflekti = (fun vorto -> (vorto, (MalantaŭModifanto, SolaFormo))) }
      ]