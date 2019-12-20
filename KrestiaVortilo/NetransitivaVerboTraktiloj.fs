namespace KrestiaVortilo

open Vorttipo

module NetransitivaVerboTraktiloj =
   let normaligi (infinitivo: string) (tipo: Vorttipo) =
      match infinitivo with
      | v when NombrigeblaKlasoTraktiloj.ĉuTranslativo(v) -> (NombrigeblaKlaso, Translativo)
      | v when v.EndsWith("selis") -> (NetransitivaVerbo1, Translativo)
      | v when v.EndsWith("shelish") -> (NetransitivaVerbo2, Translativo)
      | v when v.EndsWith("tes") -> (TransitivaVerbo2, PartaAkuzativo)
      | v when v.EndsWith("pesh") -> (TransitivaVerbo3, PartaAkuzativo)
      | v when v.EndsWith("dis") -> (Pridiranto, Translativo)
      | v when v.EndsWith("tos") -> (TransitivaVerbo2, Pasivigo)
      | v when v.EndsWith("posh") -> (TransitivaVerbo3, Pasivigo)
      | _ -> (tipo, Infinitivo)

   let tipoDeInfinitivo (infinitivo: string) =
      if infinitivo.EndsWith("s")
      then NetransitivaVerbo1
      else NetransitivaVerbo2

   let trakiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if vorto.EndsWith("s") &&
              not (["las"; "elis"; "tes"; "dis"; "tos"]
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo)))
           then Some (NetransitivaVerbo1, Infinitivo)
           elif vorto.EndsWith("sh") &&
              not (["shelish"; "pesh"; "posh"]
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo)))
           then Some (NetransitivaVerbo2, Infinitivo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (tipoDeInfinitivo vorto, Infinitivo)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("se") -> Some (NetransitivaVerbo1, Progresivo)
           | v when v.EndsWith("she") -> Some (NetransitivaVerbo2, Progresivo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 1)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto -> vorto.EndsWith("so")
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