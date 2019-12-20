﻿namespace KrestiaVortilo

open Vorttipo
open NombrigeblaKlasoTraktiloj

module NenombrigeblaKlasoTraktiloj =
   let nenombrigeblaInfinitivoFinaĵoj = [ "mu"; "mo"; "maa"; "nu"; "no"; "naa" ]
   let nenombrigeblaNekonitaNombroFinaĵoj = [ "mi"; "me"; "ma"; "nu"; "no"; "na" ]

   let predikativoEstiFinaĵoj =
      nenombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "wa")
   let atributativoEstiFinaĵoj =
      nenombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ga")
   let predikativoHaviFinaĵoj =
      nenombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ra")
   let atributativoHaviFinaĵoj =
      nenombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "re")
   let translativoFinaĵoj =
      nenombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "las")
   let ĝerundoFinaĵoj =
      nenombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "va")

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
           then Some (NenombrigeblaKlaso, NekonitaNombro)
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
           then Some (NenombrigeblaKlaso, AtributativoEsti)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaKlaso, Infinitivo)) }

      { Kontroli = fun vorto ->
           if predikativoHaviFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NenombrigeblaKlaso, PredikativoHavi)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaKlaso, Infinitivo)) }

      { Kontroli = fun vorto ->
           if atributativoHaviFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NenombrigeblaKlaso, AtributativoHavi)
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
           if ĝerundoFinaĵoj
              |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
           then Some (NenombrigeblaKlaso, Ĝerundo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 3), (NenombrigeblaKlaso, Infinitivo)) } ]