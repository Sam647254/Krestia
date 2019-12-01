namespace KrestiaVortilo

open Vorttipo

module TranslativaVerboTraktiloj =
   let infinitivoFinaĵoj = [ "p"; "t"; "k" ]
   let progresivoFinaĵoj = infinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "re")
   let perfektoFinaĵoj = infinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ro")
   let atributavioEstiFinaĵoj = infinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "rie")
   let imperativoFinaĵoj = infinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ri")
   let agantoFinaĵoj = infinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "etio")
   let ĝerundoFinaĵoj = infinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ema")
   let partaNominativoFinaĵoj = infinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "eg")
   let partaAkuzativoFinaĵoj = infinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "es")
   let estontecoFinaĵoj = infinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ela")
   let nominativoVoloFinaĵoj = infinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "era")
   let akuzativoVoloFinaĵoj = infinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ere")
   let dativoVoloFinaĵoj = infinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "eri")

   let ĉuPartaAkuzativo (infinitivo: string) =
      partaAkuzativoFinaĵoj |> List.exists (fun finaĵo -> infinitivo.EndsWith(finaĵo))

   let normaligi (infinitivo: string) =
      match infinitivo with
      | v when v.EndsWith("elit") -> (TransitivaVerbo, Translativo)
      | _ -> (TransitivaVerbo, Infinitivo)

   let trakiloj: Vorttraktilo list = [
      { Formo = (TransitivaVerbo, Infinitivo)
        Kontroli = fun vorto ->
           infinitivoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo)) &&
           not (vorto.EndsWith("elit"))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto -> (vorto, (TransitivaVerbo, Infinitivo)) }

      { Formo = (TransitivaVerbo, Progresivo)
        Kontroli = fun vorto -> progresivoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (TransitivaVerbo, Perfekto)
        Kontroli = fun vorto -> perfektoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (TransitivaVerbo, Estonteco)
        Kontroli = fun vorto -> estontecoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (TransitivaVerbo, NominativoVolo)
        Kontroli = fun vorto -> nominativoVoloFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (TransitivaVerbo, AkuzativoVolo)
        Kontroli = fun vorto -> akuzativoVoloFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (TransitivaVerbo, DativoVolo)
        Kontroli = fun vorto -> dativoVoloFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (TransitivaVerbo, AtributativoEsti)
        Kontroli = fun vorto -> atributavioEstiFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (TransitivaVerbo, Imperativo)
        Kontroli = fun vorto -> imperativoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (TransitivaVerbo, Patiento)
        Kontroli = fun vorto -> vorto.EndsWith("oniaa")
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 5)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (TransitivaVerbo, Aganto)
        Kontroli = fun vorto -> agantoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 4)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (TransitivaVerbo, Translativo)
        Kontroli = fun vorto -> vorto.EndsWith("elit")
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 4)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (TransitivaVerbo, Ĝerundo)
        Kontroli = fun vorto -> ĝerundoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (TransitivaVerbo, PartaNominativo)
        Kontroli = fun vorto -> partaNominativoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }

      { Formo = (TransitivaVerbo, PartaAkuzativo)
        Kontroli = fun vorto -> partaAkuzativoFinaĵoj |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto) }
      ]