namespace KrestiaVortilo

open Vorttipo
open System

module FremdaVortoTraktiloj =
   
   let traktiloj: Vorttraktilo list = [
      { Formo = (FremdaVorto, SolaFormo)
        Kontroli = (fun vorto -> not(Char.IsLower(vorto.[0])))
        Inflekti = neinflektebla
        Malinflekti = (fun vorto -> (vorto, (FremdaVorto, SolaFormo))) }
      ]