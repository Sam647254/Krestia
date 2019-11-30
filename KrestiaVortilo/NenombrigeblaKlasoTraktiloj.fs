namespace KrestiaVortilo

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

   let verbaĜerundoFinaĵoj = [ "pema"; "tema"; "kema"; "sema"; "mena"; "gema"; "nema" ]

   let trakiloj: Vorttraktilo list = [
      { Formo = (NenombrigeblaKlaso, Infinitivo)
        Kontroli = fun vorto ->
           nenombrigeblaInfinitivoFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto -> (vorto, (NenombrigeblaKlaso, Infinitivo)) }

      { Formo = (NenombrigeblaKlaso, NekonitaNombro)
        Kontroli = fun vorto ->
           nenombrigeblaNekonitaNombroFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo)) &&
           not (verbaĜerundoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo)))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = infinitivigi vorto
           (malinflektitaVorto, (NenombrigeblaKlaso, Infinitivo)) }

      { Formo = (NenombrigeblaKlaso, PredikativoEsti)
        Kontroli = fun vorto ->
           predikativoEstiFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaKlaso, Infinitivo)) }

      { Formo = (NenombrigeblaKlaso, AtributativoEsti)
        Kontroli = fun vorto ->
           atributativoEstiFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaKlaso, Infinitivo)) }

      { Formo = (NenombrigeblaKlaso, PredikativoHavi)
        Kontroli = fun vorto ->
           predikativoHaviFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaKlaso, Infinitivo)) }

      { Formo = (NenombrigeblaKlaso, AtributativoHavi)
        Kontroli = fun vorto ->
           atributativoHaviFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 2), (NenombrigeblaKlaso, Infinitivo)) }

      { Formo = (NenombrigeblaKlaso, Translativo)
        Kontroli = fun vorto ->
           translativoFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 3), (NenombrigeblaKlaso, Infinitivo)) }

      { Formo = (NenombrigeblaKlaso, Ĝerundo)
        Kontroli = fun vorto ->
           ĝerundoFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto -> (vorto.Substring(0, vorto.Length - 3), (NenombrigeblaKlaso, Infinitivo)) } ]