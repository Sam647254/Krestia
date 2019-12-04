namespace KrestiaVortilo

open Vorttipo

module NetransitivaVerboTraktiloj =
   let normaligi (infinitivo: string) =
      match infinitivo with
      | v when NombrigeblaKlasoTraktiloj.ĉuTranslativo(v) -> (NombrigeblaKlaso, Translativo)
      | v when v.EndsWith("elis") -> (NetransitivaVerbo, Translativo)
      | v when TransitivaVerboTraktiloj.ĉuPartaAkuzativo(v) -> (TransitivaVerbo, PartaAkuzativo)
      | v when v.EndsWith("dis") -> (Pridiranto, Translativo)
      | v when TransitivaVerboTraktiloj.ĉuPasivigo(v) -> (TransitivaVerbo, Pasivigo)
      | _ -> (NetransitivaVerbo, Infinitivo)

   let trakiloj: Vorttraktilo list = [
      { Formo = (NetransitivaVerbo, Infinitivo)
        Kontroli = fun vorto ->
           vorto.EndsWith("s") &&
           not (["las"; "elis"; "pes"; "kes"; "tes"; "dis"]
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo)))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto -> (vorto, (NetransitivaVerbo, Infinitivo)) }

      { Formo = (NetransitivaVerbo, Progresivo)
        Kontroli = fun vorto -> vorto.EndsWith("se")
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 1)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NetransitivaVerbo, Perfekto)
        Kontroli = fun vorto -> vorto.EndsWith("so")
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 1)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NetransitivaVerbo, Estonteco)
        Kontroli = fun vorto -> vorto.EndsWith("sela")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NetransitivaVerbo, NominativoVolo)
        Kontroli = fun vorto -> vorto.EndsWith("sera")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NetransitivaVerbo, DativoVolo)
        Kontroli = fun vorto -> vorto.EndsWith("seri")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NetransitivaVerbo, AtributativoEsti)
        Kontroli = fun vorto -> vorto.EndsWith("sie")
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NetransitivaVerbo, Imperativo)
        Kontroli = fun vorto -> vorto.EndsWith("sea")
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 1)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NetransitivaVerbo, Aganto)
        Kontroli = fun vorto -> vorto.EndsWith("setio")
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 4)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NetransitivaVerbo, Translativo)
        Kontroli = fun vorto -> vorto.EndsWith("selis")
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 4)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NetransitivaVerbo, Ĝerundo)
        Kontroli = fun vorto -> vorto.EndsWith("sema")
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NetransitivaVerbo, PartaNominativo)
        Kontroli = fun vorto -> vorto.EndsWith("sem") || vorto.EndsWith("seg")
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }
      ]