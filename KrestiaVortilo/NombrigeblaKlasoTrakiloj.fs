namespace KrestiaVortilo

open Vorttipo

module NombrigeblaKlasoTrakiloj =

   let nekAlInfFinaĵoj =
      [ ('i', "u")
        ('e', "o")
        ('a', "aa") ]
      |> Map.ofList

   let nombrigeblaInfinitivoFinaĵoj =
      [ "pu"; "po"; "paa"; "tu"; "to"; "taa"; "ku"; "ko"; "kaa"; "etio"; "oniaa" ]
   let nombrigeblaNekonitaNombroFinaĵoj =
      [ "pi"; "pe"; "pa"; "ti"; "te"; "ta"; "ki"; "ke"; "ka"; "etie"; "onia" ]

   let transitivaAgantoFinaĵoj = [ "petio"; "tetio"; "ketio" ]
   let nombrigeblaPredikativoEstiFinaĵoj =
      nombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "wa")
   let nombrigeblaAtributivoEstiFinaĵoj =
      nombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ga")
   let nombrigeblaPredikativoHaviFinaĵoj =
      nombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "tra")
   let nombrigeblaAtributivoHaviFinaĵoj =
      nombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "tre")
   let translativoFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "las")
   let ĝerundoFinaĵoj =
      nombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "va")

   let ĉuNetransitiaVerboAganto (infinitivo: string) = infinitivo.EndsWith("setio")
   let ĉuTransitivaVerboAganto (infinitivo: string) =
      transitivaAgantoFinaĵoj |> List.exists (fun finaĵo -> infinitivo.EndsWith(finaĵo))
   let ĉuTransitivaVerboPatiento (infinitivo: string) = infinitivo.EndsWith("oniaa")

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
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto -> (vorto, (NombrigeblaKlaso, Infinitivo)) }

      { Formo = (NombrigeblaKlaso, NekonitaNombro)
        Kontroli = fun s ->
           nombrigeblaNekonitaNombroFinaĵoj
           |> List.exists (fun finaĵo -> s.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = infinitivigi vorto
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, UnuNombro)
        Kontroli = fun vorto -> vorto.EndsWith("si")
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, PluraNombro)
        Kontroli = fun vorto -> vorto.EndsWith("ve")
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, PredikativoEsti)
        Kontroli = fun vorto ->
           nombrigeblaPredikativoEstiFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, AtributativoEsti)
        Kontroli = fun vorto ->
           nombrigeblaAtributivoEstiFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, PredikativoHavi)
        Kontroli = fun vorto ->
           nombrigeblaPredikativoHaviFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, AtributativoHavi)
        Kontroli = fun vorto ->
           nombrigeblaAtributivoHaviFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, Translativo)
        Kontroli = fun vorto ->
           translativoFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (NombrigeblaKlaso, Ĝerundo)
        Kontroli = fun vorto ->
           ĝerundoFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2) |> infinitivigi
           (malinflektitaVorto, normaligi malinflektitaVorto) } ]