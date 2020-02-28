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
         if vorto.EndsWith("dre") then Some (NombrigeblaEco, Difinito) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (infinitivigi vorto, (NombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dresi")
           then Some (NombrigeblaEco, UnuNombro)
           elif vorto.EndsWith("dreve")
           then Some (NombrigeblaEco, PluraNombro)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (infinitivigi (vorto.Substring(0, vorto.Length - 2)), (NombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dresinsa")
           then Some (NombrigeblaEco, UnuHavaĵo)
           elif vorto.EndsWith("drevensa")
           then Some (NombrigeblaEco, PluraHavaĵo)
           elif vorto.EndsWith("drensa")
           then Some (NombrigeblaEco, Havaĵo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (infinitivigi (vorto.Substring(0, vorto.Length - 2)), (NombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("gro") then Some (NenombrigeblaEco, Infinitivo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (NenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("gre") then Some (NenombrigeblaEco, Difinito) else None
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
         if vorto.EndsWith("drowa") then Some (NombrigeblaEco, PredikativoEsti) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (NombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("groga") then Some (NenombrigeblaEco, AtributativoEstiMalantaŭ) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("droga") then Some (NombrigeblaEco, AtributativoEstiMalantaŭ) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 2), (NombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("grelas") then Some (NenombrigeblaEco, Translativo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (NenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("drelas") then Some (NombrigeblaEco, Translativo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (NombrigeblaEco, Infinitivo)) }


      { Kontroli = fun vorto ->
           if vorto.EndsWith("drerim") then Some (NombrigeblaEco, Ekzistado) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (NombrigeblaEco, Ekzistado)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dresirim") then Some (NombrigeblaEco, UnuEkzistado) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 5) |> infinitivigi,
              (NombrigeblaEco, UnuEkzistado)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dreverim") then Some (NombrigeblaEco, PluraEkzistado) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 5) |> infinitivigi,
              (NombrigeblaEco, PluraEkzistado)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("greva") then Some (NenombrigeblaEco, Ĝerundo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (NenombrigeblaEco, Infinitivo)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("dreva") then Some (NombrigeblaEco, Ĝerundo) else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto.Substring(0, vorto.Length - 3) |> infinitivigi,
              (NombrigeblaEco, Infinitivo)) }
      ]