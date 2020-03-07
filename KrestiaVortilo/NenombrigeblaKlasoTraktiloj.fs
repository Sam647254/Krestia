namespace KrestiaVortilo

open Vorttipo
open NombrigeblaKlasoTraktiloj

module NenombrigeblaKlasoTraktiloj =
   let nenombrigeblaInfinitivoFinaĵoj = [ "mu"; "mo"; "maa"; "nu"; "no"; "naa" ]
   let nenombrigeblaNekonitaNombroFinaĵoj = [ "mi"; "me"; "ma"; "nu"; "no"; "na" ]

   let havaĵoFinaĵoj =
      nenombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "nsa")
   let predikativoEstiFinaĵoj =
      nenombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "wa")
   let atributativoEstiFinaĵoj =
      nenombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ga")
   let predikativoHaviFinaĵoj =
      nenombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ris")
   let translativoFinaĵoj =
      nenombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "las")
   let ĝerundoFinaĵoj =
      nenombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "va")
   let ekzistadoFinaĵoj =
      nenombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "rim")

   let verbaĜerundoFinaĵoj = [ "pema"; "tema"; "sema"; "mena"; "igema"; "egema"; "nema" ]

   let trakiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if nenombrigeblaInfinitivoFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NenombrigeblaKlaso, Infinitivo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto, (NenombrigeblaKlaso, Infinitivo)) }

      { Kontroli = fun vorto ->
           if nenombrigeblaNekonitaNombroFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo)) &&
                 not (verbaĜerundoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo)))
           then Some (NenombrigeblaKlaso, Difinito)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = infinitivigi vorto
           (malinflektitaVorto, (NenombrigeblaKlaso, Infinitivo)) }

      { Kontroli = fun vorto ->
         if havaĵoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
         then Some (NenombrigeblaKlaso, Havaĵo)
         else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = infinitivigi vorto
           (malinflektitaVorto, (NenombrigeblaKlaso, Infinitivo)) }

      { Kontroli = fun vorto ->
           if predikativoEstiFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NenombrigeblaKlaso, PredikativoEsti)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaKlaso, Infinitivo)) }

      { Kontroli = fun vorto ->
           if atributativoEstiFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NenombrigeblaKlaso, AtributativoEstiMalantaŭ)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaKlaso, Infinitivo)) }

      { Kontroli = fun vorto ->
           if predikativoHaviFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NenombrigeblaKlaso, Havado)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaKlaso, Infinitivo)) }

      { Kontroli = fun vorto ->
           if translativoFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NenombrigeblaKlaso, Translativo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 3), (NenombrigeblaKlaso, Infinitivo)) }

      { Kontroli = fun vorto ->
           if ekzistadoFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NenombrigeblaKlaso, Ekzistado)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Kontroli = fun vorto ->
           if ĝerundoFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NenombrigeblaKlaso, Ĝerundo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 3), (NenombrigeblaKlaso, Infinitivo)) } ]