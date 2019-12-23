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

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("so") -> Some (NetransitivaVerbo1, Perfekto)
           | _ when vorto.EndsWith("sho") -> Some (NetransitivaVerbo2, Perfekto)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 1)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("sela") -> Some (NetransitivaVerbo1, Estonteco)
           | _ when vorto.EndsWith("shela") -> Some (NetransitivaVerbo2, Estonteco)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("sera") -> Some (NetransitivaVerbo1, NominativoVolo)
           | _ when vorto.EndsWith("shera") -> Some (NetransitivaVerbo2, NominativoVolo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("sheri")
           then Some (NetransitivaVerbo2, DativoVolo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto NetransitivaVerbo2) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("sie") -> Some (NetransitivaVerbo1, AtributativoEsti)
           | _ when vorto.EndsWith("shie") -> Some (NetransitivaVerbo2, AtributativoEsti)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("sea") -> Some (NetransitivaVerbo1, Imperativo)
           | _ when vorto.EndsWith("shea") -> Some (NetransitivaVerbo2, Imperativo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 1)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("setio") -> Some (NetransitivaVerbo1, Aganto)
           | _ when vorto.EndsWith("shetio") -> Some (NetransitivaVerbo2, Aganto)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 4)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("selis") -> Some (NetransitivaVerbo1, Translativo)
           | _ when vorto.EndsWith("shelish") -> Some (NetransitivaVerbo2, Translativo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("selis") ->
              let malinflektitaVorto = vorto.Substring(0, vorto.Length - 4)
              (malinflektitaVorto, normaligi malinflektitaVorto NetransitivaVerbo1)
           | _ ->
              let malinflektitaVorto = vorto.Substring(0, vorto.Length - 5)
              (malinflektitaVorto, normaligi malinflektitaVorto NetransitivaVerbo2) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("sema") -> Some (NetransitivaVerbo1, Ĝerundo)
           | _ when vorto.EndsWith("shema") -> Some (NetransitivaVerbo2, Ĝerundo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("sem") -> Some (NetransitivaVerbo1, PartaNominativo)
           | _ when vorto.EndsWith("shen") -> Some (NetransitivaVerbo2, PartaNominativo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }
      ]