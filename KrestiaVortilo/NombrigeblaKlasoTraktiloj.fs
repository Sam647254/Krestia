namespace KrestiaVortilo

open Vorttipo

module NombrigeblaKlasoTraktiloj =

   let nekAlInfFinaĵoj =
      [ ('i', "u")
        ('e', "o")
        ('a', "aa") ]
      |> Map.ofList

   let nombrigeblaInfinitivoFinaĵoj =
      [ "pu"; "po"; "paa"; "tu"; "to"; "taa"; "ku"; "ko"; "kaa" ]
   let nombrigeblaNekonitaNombroFinaĵoj =
      [ "pi"; "pe"; "pa"; "ti"; "te"; "ta"; "ki"; "ke"; "ka"; "onia"; "etie"; "orie" ]
   let nombrigeblaUnuNombroFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "si")
   let nombrigeblaPluraNombroFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ve")
   let havaĵoFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj @
      nombrigeblaUnuNombroFinaĵoj @
      nombrigeblaPluraNombroFinaĵoj
      |> List.map (fun finaĵo -> finaĵo + "nsa")

   let agantoFinaĵoj = [ "petio"; "tetio"; "ketio"; "setio" ]
   let nombrigeblaPredikativoEstiFinaĵoj =
      nombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "wa")
   let nombrigeblaAtributivoEstiFinaĵoj =
      nombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ga")
   let nombrigeblaPredikativoHaviFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj @
      nombrigeblaUnuNombroFinaĵoj @
      nombrigeblaPluraNombroFinaĵoj
      |> List.map (fun finaĵo -> finaĵo + "ra")
   let nombrigeblaAtributivoHaviFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj @
      nombrigeblaUnuNombroFinaĵoj @
      nombrigeblaPluraNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "re")
   let translativoFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "las")
   let ĝerundoFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "va")

   let ĉuNetransitiaVerboAganto (infinitivo: string) = infinitivo.EndsWith("setio")
   let ĉuTransitivaVerboAganto (infinitivo: string) =
      agantoFinaĵoj |> List.exists (fun finaĵo -> infinitivo.EndsWith(finaĵo))
   let ĉuTransitivaVerboPatiento (infinitivo: string) = infinitivo.EndsWith("oniaa")
   let ĉuTranslativo (infinitivo: string) =
      translativoFinaĵoj |> List.exists (fun finaĵo -> infinitivo.EndsWith(finaĵo))

   let normaligi infinitivo =
      match infinitivo with
      | v when ĉuNetransitiaVerboAganto v -> (NetransitivaVerbo, Aganto)
      | v when ĉuTransitivaVerboAganto v -> (TransitivaVerbo, Aganto)
      | v when ĉuTransitivaVerboPatiento v -> (TransitivaVerbo, Patiento)
      | _ -> (NombrigeblaKlaso, Infinitivo)

   let infinitivigi (nekonitaNombro: string) =
      nekonitaNombro.Substring(0, nekonitaNombro.Length - 1) +
      nekAlInfFinaĵoj.[nekonitaNombro.Chars(nekonitaNombro.Length - 1)]

   let trakiloj: Vorttraktilo list = [
      { Formo = (NombrigeblaKlaso, Infinitivo)
        Kontroli = fun vorto ->
          nombrigeblaInfinitivoFinaĵoj
          |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (NombrigeblaKlaso, Infinitivo)) }

      { Formo = (NombrigeblaKlaso, NekonitaNombro)
        Kontroli = fun s ->
           nombrigeblaNekonitaNombroFinaĵoj
           |> List.exists (fun finaĵo -> s.EndsWith(finaĵo))
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = infinitivigi vorto
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, UnuNombro)
        Kontroli = fun vorto -> vorto.EndsWith("si")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, PluraNombro)
        Kontroli = fun vorto -> vorto.EndsWith("ve")
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, Havaĵo)
        Kontroli = fun vorto -> havaĵoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, PredikativoEsti)
        Kontroli = fun vorto ->
           nombrigeblaPredikativoEstiFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, AtributativoEsti)
        Kontroli = fun vorto ->
           nombrigeblaAtributivoEstiFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, PredikativoHavi)
        Kontroli = fun vorto ->
           nombrigeblaPredikativoHaviFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, AtributativoHavi)
        Kontroli = fun vorto ->
           nombrigeblaAtributivoHaviFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, Translativo)
        Kontroli = fun vorto ->
           translativoFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, Ĝerundo)
        Kontroli = fun vorto ->
           ĝerundoFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) } ]