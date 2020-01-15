namespace KrestiaVortilo

open Vorttipo

module MalplenaVerboTraktiloj =
   let normaligi (infinitivo: string) =
      let ĉuNombrigeblaKlasoEkzistado (infinitivo: string) =
         NombrigeblaKlasoTraktiloj.ekzistadoFinaĵoj
         |> List.exists (fun finaĵo -> infinitivo.EndsWith(finaĵo))

      match infinitivo with
      | _ when infinitivo.EndsWith("sem") -> (NetransitivaVerbo1, PartaNominativo)
      | _ when infinitivo.EndsWith("melim") -> (MalplenaVerbo, Translativo)
      | _ when infinitivo.EndsWith("igam") -> (PartaTransitivaVerbo1, PartaAkuzativo)
      | _ when infinitivo.EndsWith("nom") -> (PartaNetransitivaVerbo, PartaDativo)
      | _ when ĉuNombrigeblaKlasoEkzistado infinitivo -> (NombrigeblaKlaso, Ekzistado)
      | _ -> (MalplenaVerbo, Infinitivo)

   let traktiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
         if vorto.EndsWith("m") && (not(vorto.EndsWith("sem") || vorto.EndsWith("nom")
            || vorto.EndsWith("melim") || vorto.EndsWith("nom")))
         then Some (MalplenaVerbo, Infinitivo)
         else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (MalplenaVerbo, Infinitivo)) }
        
      { Kontroli = fun vorto ->
           if vorto.EndsWith("mia")
           then Some (MalplenaVerbo, Progresivo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 2)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("mio")
           then Some (MalplenaVerbo, Perfekto)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 2)
           (malinflektita, normaligi malinflektita) }
      
      { Kontroli = fun vorto ->
           if vorto.EndsWith("mela")
           then Some (MalplenaVerbo, Estonteco)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 3)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("melim")
           then Some (MalplenaVerbo, Translativo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 4)
           (malinflektita, normaligi malinflektita) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("mena")
           then Some (MalplenaVerbo, Ĝerundo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektita = vorto.Substring(0, vorto.Length - 3)
           (malinflektita, normaligi malinflektita) }]