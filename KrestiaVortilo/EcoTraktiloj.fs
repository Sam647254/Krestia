namespace KrestiaVortilo

open Vorttipo

module EcoTraktiloj =

   let infinitivigi (nekonitaNombro: string) =
      nekonitaNombro.Substring(0, nekonitaNombro.Length - 1) + "o"

   let traktiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if vorto.EndsWith("dro") then Some (NombrigeblaEco, Infinitivo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (NombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
         if vorto.EndsWith("dre") then Some (NombrigeblaEco, NekonitaNombro) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (infinitivigi vorto, (NombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("gro") then Some (NenombrigeblaEco, Infinitivo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (NenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("gre") then Some (NenombrigeblaEco, NekonitaNombro) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
          (infinitivigi vorto, (NenombrigeblaEco, Infinitivo)) }
      
      { Kontroli = fun vorto ->
           if vorto.EndsWith("grensa") then Some (NenombrigeblaEco, Havaĵo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (NenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("growa") then Some (NenombrigeblaEco, PredikativoEsti) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("groga") then Some (NenombrigeblaEco, AtributativoEsti) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("grelas") then Some (NenombrigeblaEco, Translativo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (NenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("greva") then Some (NenombrigeblaEco, Ĝerundo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (NenombrigeblaEco, Infinitivo)) }
      ]