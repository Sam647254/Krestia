namespace KrestiaVortilo

open Vorttipo

module TransitivaVerboTraktiloj =
   let normaligi (infinitivo: string) (tipo: Vorttipo) =
      match infinitivo with
      | v when v.EndsWith("telit") -> (TransitivaVerbo2, Translativo)
      | v when v.EndsWith("pelit") -> (TransitivaVerbo3, Translativo)
      | _ -> (tipo, Infinitivo)

   let tipoDe (infinitivo: string) =
      match infinitivo with
      | v when v.EndsWith("t") -> Some TransitivaVerbo2
      | v when v.EndsWith("p") -> Some TransitivaVerbo3
      | _ -> None

   let tipoDeInfinitivo (infinitivo: string) =
      if infinitivo.EndsWith("t")
      then TransitivaVerbo2
      else TransitivaVerbo3

   let trakiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if not (vorto.EndsWith("elit")) && not (vorto.EndsWith("det"))
           then tipoDe vorto |> Option.map (fun tipo -> (tipo, Infinitivo))
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto, ((if vorto.EndsWith("t") then TransitivaVerbo2 else TransitivaVerbo3), Infinitivo)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tre") -> Some (TransitivaVerbo2, Progresivo)
           | v when v.EndsWith("pre") -> Some (TransitivaVerbo3, Progresivo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tro") -> Some (TransitivaVerbo2, Perfekto)
           | v when v.EndsWith("pro") -> Some (TransitivaVerbo3, Perfekto)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tela") -> Some (TransitivaVerbo2, Estonteco)
           | v when v.EndsWith("pela") -> Some (TransitivaVerbo3, Estonteco)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tora") -> Some (TransitivaVerbo2, NominativoVolo)
           | v when v.EndsWith("pora") -> Some (TransitivaVerbo3, NominativoVolo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tore") -> Some (TransitivaVerbo2, AkuzativoVolo)
           | v when v.EndsWith("pore") -> Some (TransitivaVerbo3, AkuzativoVolo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("peri")
           then Some (TransitivaVerbo3, DativoVolo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto TransitivaVerbo3) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("trie") -> Some (TransitivaVerbo2, AtributativoEstiMalantaŭ)
           | v when v.EndsWith("prie") -> Some (TransitivaVerbo3, AtributativoEstiMalantaŭ)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tri") -> Some (TransitivaVerbo2, Imperativo)
           | v when v.EndsWith("pri") -> Some (TransitivaVerbo3, Imperativo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("toniaa") -> Some (TransitivaVerbo2, Patiento)
           | v when v.EndsWith("poniaa") -> Some (TransitivaVerbo3, Patiento)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 5)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tetio") -> Some (TransitivaVerbo2, Aganto)
           | v when v.EndsWith("petio") -> Some (TransitivaVerbo3, Aganto)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 4)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("telit") -> Some (TransitivaVerbo2, Translativo)
           | v when v.EndsWith("pelip") -> Some (TransitivaVerbo3, Translativo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 4)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tema") -> Some (TransitivaVerbo2, Ĝerundo)
           | v when v.EndsWith("pema") -> Some (TransitivaVerbo3, Ĝerundo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tig") -> Some (TransitivaVerbo2, PartaNominativo)
           | v when v.EndsWith("peg") -> Some (TransitivaVerbo3, PartaNominativo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tes") -> Some (TransitivaVerbo2, PartaAkuzativo)
           | v when v.EndsWith("pesh") -> Some (TransitivaVerbo3, PartaAkuzativo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto =
              if vorto.EndsWith("tes")
              then vorto.Substring(0, vorto.Length - 2)
              else vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tos") -> Some (TransitivaVerbo2, Pasivigo)
           | v when v.EndsWith("posh") -> Some (TransitivaVerbo3, Pasivigo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto TransitivaVerbo3) }
      ]