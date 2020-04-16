namespace KrestiaVortilo

open Vorttipo

module NetransitivaVerboTraktiloj =
   let normaligi (infinitivo: string) (tipo: Vorttipo) =
      match infinitivo with
      | v when NombrigeblaKlasoTraktiloj.ĉuTranslativo(v) -> (NombrigeblaKlaso, Translativo)
      | v when v.EndsWith("selis") -> (NetransitivaVerbo, Translativo)
      | v when v.EndsWith("shelish") -> (NedirektaTransitivaVerbo, Translativo)
      | v when v.EndsWith("tes") -> (TransitivaVerbo, PartaUjo2)
      | v when v.EndsWith("pesh") -> (DutransitivaVerbo, PartaUjo2)
      | v when v.EndsWith("dis") -> (Pridiranto, Translativo)
      | v when v.EndsWith("tos") -> (TransitivaVerbo, Pasivigo)
      | v when v.EndsWith("posh") -> (DutransitivaVerbo, Pasivigo)
      | _ -> (tipo, Infinitivo)

   let tipoDeInfinitivo (infinitivo: string) =
      if infinitivo.EndsWith("s")
      then NetransitivaVerbo
      else NedirektaTransitivaVerbo

   let trakiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if vorto.EndsWith("s") &&
              not (["las"; "elis"; "tes"; "dis"; "tos"; "ris"]
                   |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo)))
           then Some (NetransitivaVerbo, Infinitivo)
           elif vorto.EndsWith("sh") &&
              not (["shelish"; "pesh"; "posh"]
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo)))
           then Some (NedirektaTransitivaVerbo, Infinitivo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (tipoDeInfinitivo vorto, Infinitivo)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("se") -> Some (NetransitivaVerbo, Progresivo)
           | v when v.EndsWith("she") -> Some (NedirektaTransitivaVerbo, Progresivo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 1)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("so") -> Some (NetransitivaVerbo, Perfekto)
           | _ when vorto.EndsWith("sho") -> Some (NedirektaTransitivaVerbo, Perfekto)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 1)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("sela") -> Some (NetransitivaVerbo, Estonteco)
           | _ when vorto.EndsWith("shela") -> Some (NedirektaTransitivaVerbo, Estonteco)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("sera") -> Some (NetransitivaVerbo, Ujo1Volo)
           | _ when vorto.EndsWith("shera") -> Some (NedirektaTransitivaVerbo, Ujo1Volo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("sheri")
           then Some (NedirektaTransitivaVerbo, Ujo3Volo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto NedirektaTransitivaVerbo) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("sie") -> Some (NetransitivaVerbo, AtributivoEstiMalantaŭ)
           | _ when vorto.EndsWith("shie") -> Some (NedirektaTransitivaVerbo, AtributivoEstiMalantaŭ)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("sea") -> Some (NetransitivaVerbo, Imperativo)
           | _ when vorto.EndsWith("shea") -> Some (NedirektaTransitivaVerbo, Imperativo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 1)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("setio") -> Some (NetransitivaVerbo, Argumento1)
           | _ when vorto.EndsWith("shetio") -> Some (NedirektaTransitivaVerbo, Argumento1)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 4)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("selis") -> Some (NetransitivaVerbo, Translativo)
           | _ when vorto.EndsWith("shelish") -> Some (NedirektaTransitivaVerbo, Translativo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("selis") ->
              let malinflektitaVorto = vorto.Substring(0, vorto.Length - 4)
              (malinflektitaVorto, normaligi malinflektitaVorto NetransitivaVerbo)
           | _ ->
              let malinflektitaVorto = vorto.Substring(0, vorto.Length - 5)
              (malinflektitaVorto, normaligi malinflektitaVorto NedirektaTransitivaVerbo) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("sema") -> Some (NetransitivaVerbo, Ĝerundo)
           | _ when vorto.EndsWith("shema") -> Some (NedirektaTransitivaVerbo, Ĝerundo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | _ when vorto.EndsWith("sem") -> Some (NetransitivaVerbo, PartaUjo1)
           | _ when vorto.EndsWith("shen") -> Some (NedirektaTransitivaVerbo, PartaUjo1)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }
      ]