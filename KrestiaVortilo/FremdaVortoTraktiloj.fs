﻿namespace KrestiaVortilo

open Vorttipo
open System

module FremdaVortoTraktiloj =
   
   let traktiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if vorto.Length > 0 && not(Char.IsLower(vorto.[0]))
           then Some (FremdaVorto, SolaFormo)
           else None
        Inflekti = neinflektebla
        Malinflekti = (fun vorto -> (vorto, (FremdaVorto, SolaFormo))) }
      ]