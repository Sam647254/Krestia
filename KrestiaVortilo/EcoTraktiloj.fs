namespace KrestiaVortilo

open Vorttipo

module EcoTraktiloj =

   let infinitivigi (nekonitaNombro: string) =
      nekonitaNombro.Substring(0, nekonitaNombro.Length - 1) + "o"

   let traktiloj: Vorttraktilo list = [
      { Formo = (NombrigeblaEco, Infinitivo)
        Kontroli = fun vorto -> vorto.EndsWith("dro")
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (NombrigeblaEco, Infinitivo)) }

      { Formo = (NenombrigeblaEco, Infinitivo)
        Kontroli = fun vorto -> vorto.EndsWith("gro")
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (NenombrigeblaEco, Infinitivo)) }

      { Formo = (NenombrigeblaEco, NekonitaNombro)
        Kontroli = fun vorto -> vorto.EndsWith("gre")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
          (infinitivigi vorto, (NenombrigeblaEco, Infinitivo)) }
      
      { Formo = (NenombrigeblaEco, Havaĵo)
        Kontroli = fun vorto -> vorto.EndsWith("grensa")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (NenombrigeblaEco, Infinitivo)) }

      { Formo = (NenombrigeblaEco, PredikativoEsti)
        Kontroli = fun vorto -> vorto.EndsWith("growa")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaEco, Infinitivo)) }

      { Formo = (NenombrigeblaEco, AtributativoEsti)
        Kontroli = fun vorto -> vorto.EndsWith("groga")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaEco, Infinitivo)) }

      { Formo = (NenombrigeblaEco, Translativo)
        Kontroli = fun vorto -> vorto.EndsWith("grelas")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (NenombrigeblaEco, Infinitivo)) }

      { Formo = (NenombrigeblaEco, Ĝerundo)
        Kontroli = fun vorto -> vorto.EndsWith("greva")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (NenombrigeblaEco, Infinitivo)) }
      ]