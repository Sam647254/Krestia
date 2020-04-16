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
      [ "pi"; "pe"; "pa"; "ti"; "te"; "ta"; "ki"; "ke"; "ka"; "ponia"; "tonia"; "etie"; "orie" ]
   let nombrigeblaUnuNombroFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "si")
   let nombrigeblaPluraNombroFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ve")
   let difinitoNombrigeblaKlasoHavaĵoFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "nsa")
   let unuNombrigeblaKlasoHavaĵoFinaĵoj =
      nombrigeblaUnuNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "nsa")
   let pluraNombrigeblaKlasoHavaĵoFinaĵoj =
      nombrigeblaPluraNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "nsa")


   let nombrigeblaPredikativoEstiFinaĵoj =
      nombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "wa")
   let nombrigeblaAtributivoEstiMalantaŭFinaĵoj =
      nombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ga")
   let nombrigeblaAtributivoEstiAntaŭFinaĵoj =
      nombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "va")
   let nombrigeblaPredikativoHaviFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ris")
   let nombrigeblaPredikativoHaviUnuFinaĵoj =
      nombrigeblaUnuNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ris")
   let nombrigeblaPredikativoHaviPluraFinaĵoj =
      nombrigeblaPluraNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ris")
   let translativoFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "las")
   let ĝerundoFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "vra")
   let specifaĜerundoFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "va")
   let ekzistadoFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "rim")
   let unuEkzistadoFinaĵoj = nombrigeblaUnuNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "rim")
   let pluraEkzistadoFinaĵoj = nombrigeblaPluraNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "rim")
   let solaFinaĵoj = nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ra")
   let solaUnuFinaĵoj = nombrigeblaUnuNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ra")
   let solaPluraFinaĵoj = nombrigeblaPluraNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ra")

   let ĉuNetransitivaVerbo1Aganto (infinitivo: string) = infinitivo.EndsWith("setio")
   let ĉuNetransitivaVerbo2Aganto (infinitivo: string) = infinitivo.EndsWith("shetio")
   let ĉuTransitivaVerboPatiento (infinitivo: string) = infinitivo.EndsWith("oniaa")
   let ĉuTranslativo (infinitivo: string) =
      translativoFinaĵoj |> List.exists (fun finaĵo -> infinitivo.EndsWith(finaĵo))

   let normaligi (infinitivo: string) =
      match infinitivo with
      | v when v.EndsWith("setio") -> (NetransitivaVerbo, Argumento1)
      | v when v.EndsWith("shetio") -> (NedirektaTransitivaVerbo, Argumento1)
      | v when v.EndsWith("tetio") -> (TransitivaVerbo, Argumento1)
      | v when v.EndsWith("petio") -> (DutransitivaVerbo, Argumento1)
      | v when v.EndsWith("toniaa") -> (TransitivaVerbo, Argumento2)
      | v when v.EndsWith("poniaa") -> (DutransitivaVerbo, Argumento2)
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
           then Some (NombrigeblaKlaso, Difinito)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = infinitivigi vorto
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if nombrigeblaUnuNombroFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, UnuNombro)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if nombrigeblaPluraNombroFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, PluraNombro)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if difinitoNombrigeblaKlasoHavaĵoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, Havaĵo)
           elif unuNombrigeblaKlasoHavaĵoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, UnuHavaĵo)
           elif pluraNombrigeblaKlasoHavaĵoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, PluraHavaĵo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto =
              if vorto.EndsWith("vensa") || vorto.EndsWith("sinsa")
              then vorto.Substring(0, vorto.Length - 5)
              else vorto.Substring(0, vorto.Length - 3)
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
           if nombrigeblaAtributivoEstiMalantaŭFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, AtributivoEstiMalantaŭ)
           elif nombrigeblaAtributivoEstiAntaŭFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, AtributivoEstiAntaŭ)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if nombrigeblaPredikativoHaviFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, Havado)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if nombrigeblaPredikativoHaviUnuFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, UnuHavado)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 5) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if nombrigeblaPredikativoHaviPluraFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, PluraHavado)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 5) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if vorto |> ĉuHavasFinaĵon solaFinaĵoj
           then Some (NombrigeblaKlaso, Sola)
           elif vorto |> ĉuHavasFinaĵon solaUnuFinaĵoj
           then Some (NombrigeblaKlaso, UnuSola)
           elif vorto |> ĉuHavasFinaĵon solaPluraFinaĵoj
           then Some (NombrigeblaKlaso, PluraSola)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto =
              if vorto |> ĉuHavasFinaĵon solaFinaĵoj
              then vorto.Substring(0, vorto.Length - 2)
              else vorto.Substring(0, vorto.Length - 4)
           (malinflektitaVorto, normaligi malinflektitaVorto)}

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
           elif unuEkzistadoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, UnuEkzistado)
           elif pluraEkzistadoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, PluraEkzistado)
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
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if specifaĜerundoFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NombrigeblaKlaso, SpecifaĜerundo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) } ]