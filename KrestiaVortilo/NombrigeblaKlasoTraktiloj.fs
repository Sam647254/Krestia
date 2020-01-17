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
   let ekzistadoFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj @
      nombrigeblaUnuNombroFinaĵoj @
      nombrigeblaPluraNombroFinaĵoj
      |> List.map (fun finaĵo -> finaĵo + "rim")

   let ĉuNetransitivaVerbo1Aganto (infinitivo: string) = infinitivo.EndsWith("setio")
   let ĉuNetransitivaVerbo2Aganto (infinitivo: string) = infinitivo.EndsWith("shetio")
   let ĉuTransitivaVerboPatiento (infinitivo: string) = infinitivo.EndsWith("oniaa")
   let ĉuTranslativo (infinitivo: string) =
      translativoFinaĵoj |> List.exists (fun finaĵo -> infinitivo.EndsWith(finaĵo))

   let normaligi (infinitivo: string) =
      match infinitivo with
      | v when v.EndsWith("setio") -> (NetransitivaVerbo1, Aganto)
      | v when v.EndsWith("shetio") -> (NetransitivaVerbo2, Aganto)
      | v when v.EndsWith("tetio") -> (TransitivaVerbo2, Aganto)
      | v when v.EndsWith("petio") -> (TransitivaVerbo3, Aganto)
      | v when v.EndsWith("toniaa") -> (TransitivaVerbo2, Patiento)
      | v when v.EndsWith("poniaa") -> (TransitivaVerbo3, Patiento)
      | _ -> (NombrigeblaKlaso, Infinitivo)

   let infinitivigi (nekonitaNombro: string) =
      nekonitaNombro.Substring(0, nekonitaNombro.Length - 1) +
      nekAlInfFinaĵoj.[nekonitaNombro.Chars(nekonitaNombro.Length - 1)]

   let trakiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if nombrigeblaInfinitivoFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, Infinitivo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (NombrigeblaKlaso, Infinitivo)) }

      { Kontroli = fun s ->
           if nombrigeblaNekonitaNombroFinaĵoj
              |> List.exists (fun finaĵo -> s.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, NekonitaNombro)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = infinitivigi vorto
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("si")
           then Some (NombrigeblaKlaso, UnuNombro)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("ve")
           then Some (NombrigeblaKlaso, PluraNombro)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if havaĵoFinaĵoj |>
              List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, Havaĵo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto =
              (if vorto.EndsWith("vensa") || vorto.EndsWith("sinsa")
              then vorto.Substring(0, vorto.Length - 5)
              else vorto.Substring(0, vorto.Length - 3))
              |> normaligi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if nombrigeblaPredikativoEstiFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, PredikativoEsti)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if nombrigeblaAtributivoEstiFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, AtributativoEsti)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if nombrigeblaPredikativoHaviFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, PredikativoHavi)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto =
              (if vorto.EndsWith("vera") || vorto.EndsWith("sira")
              then vorto.Substring(0, vorto.Length - 4)
              else vorto.Substring(0, vorto.Length - 2))
              |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if nombrigeblaAtributivoHaviFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, AtributativoHavi)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto =
              (if vorto.EndsWith("vere") || vorto.EndsWith("sire")
              then vorto.Substring(0, vorto.Length - 4)
              else vorto.Substring(0, vorto.Length - 2))
              |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if translativoFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, Translativo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if ekzistadoFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, Ekzistado)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto =
              (if vorto.EndsWith("verim") || vorto.EndsWith("sirim")
              then vorto.Substring(0, vorto.Length - 5)
              else vorto.Substring(0, vorto.Length - 3))
              |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if ĝerundoFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, Ĝerundo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) } ]