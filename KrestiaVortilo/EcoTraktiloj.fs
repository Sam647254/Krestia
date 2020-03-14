namespace KrestiaVortilo

open Vorttipo

module EcoTraktiloj =

   let infinitivigi (nekonitaNombro: string) =
      nekonitaNombro.Substring(0, nekonitaNombro.Length - 1) + "o"

   let traktiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if vorto.EndsWith("dro") then Some (AntaŭNombrigeblaEco, Infinitivo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (AntaŭNombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
         if vorto.EndsWith("dre") then Some (AntaŭNombrigeblaEco, Difinito) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (infinitivigi vorto, (AntaŭNombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dresi")
           then Some (AntaŭNombrigeblaEco, UnuNombro)
           elif vorto.EndsWith("dreve")
           then Some (AntaŭNombrigeblaEco, PluraNombro)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (infinitivigi (vorto.Substring(0, vorto.Length - 2)), (AntaŭNombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dresinsa")
           then Some (AntaŭNombrigeblaEco, UnuHavaĵo)
           elif vorto.EndsWith("drevensa")
           then Some (AntaŭNombrigeblaEco, PluraHavaĵo)
           elif vorto.EndsWith("drensa")
           then Some (AntaŭNombrigeblaEco, Havaĵo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (infinitivigi (vorto.Substring(0, vorto.Length - 2)), (AntaŭNombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("gro") then Some (AntaŭNenombrigeblaEco, Infinitivo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (AntaŭNenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("gre") then Some (AntaŭNenombrigeblaEco, Difinito) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
          (infinitivigi vorto, (AntaŭNenombrigeblaEco, Infinitivo)) }
      
      { Kontroli = fun vorto ->
           if vorto.EndsWith("grensa") then Some (AntaŭNenombrigeblaEco, Havaĵo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (AntaŭNenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("growa") then Some (AntaŭNenombrigeblaEco, PredikativoEsti) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (AntaŭNenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
         if vorto.EndsWith("drowa") then Some (AntaŭNombrigeblaEco, PredikativoEsti) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (AntaŭNombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("groga") then Some (AntaŭNenombrigeblaEco, AtributivoEstiMalantaŭ) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (AntaŭNenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("droga") then Some (AntaŭNombrigeblaEco, AtributivoEstiMalantaŭ) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (AntaŭNombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("grelas") then Some (AntaŭNenombrigeblaEco, Translativo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (AntaŭNenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("drelas") then Some (AntaŭNombrigeblaEco, Translativo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (AntaŭNombrigeblaEco, Infinitivo)) }


      { Kontroli = fun vorto ->
           if vorto.EndsWith("drerim") then Some (AntaŭNombrigeblaEco, Ekzistado) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (AntaŭNombrigeblaEco, Ekzistado)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dresirim") then Some (AntaŭNombrigeblaEco, UnuEkzistado) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 5) |> infinitivigi,
              (AntaŭNombrigeblaEco, UnuEkzistado)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dreverim") then Some (AntaŭNombrigeblaEco, PluraEkzistado) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 5) |> infinitivigi,
              (AntaŭNombrigeblaEco, PluraEkzistado)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("greva") then Some (AntaŭNenombrigeblaEco, Ĝerundo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (AntaŭNenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dreva") then Some (AntaŭNombrigeblaEco, Ĝerundo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (AntaŭNombrigeblaEco, Infinitivo)) }
      ]