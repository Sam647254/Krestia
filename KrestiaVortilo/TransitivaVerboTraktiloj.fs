namespace KrestiaVortilo

open Vorttipo

module TransitivaVerboTraktiloj =
   let normaligi (infinitivo: string) (tipo: Vorttipo) =
      match infinitivo with
      | v when v.EndsWith("telit") -> (TransitivaVerbo, Translativo)
      | v when v.EndsWith("pelit") -> (DutransitivaVerbo, Translativo)
      | _ -> (tipo, Infinitivo)

   let tipoDe (infinitivo: string) =
      match infinitivo with
      | v when v.EndsWith("t") -> Some TransitivaVerbo
      | v when v.EndsWith("p") -> Some DutransitivaVerbo
      | _ -> None

   let tipoDeInfinitivo (infinitivo: string) =
      if infinitivo.EndsWith("t")
      then TransitivaVerbo
      else DutransitivaVerbo

   let trakiloj: Vorttraktilo list = [
      { Kontroli = fun vorto ->
           if not (vorto.EndsWith("elit")) && not (vorto.EndsWith("det"))
           then tipoDe vorto |> Option.map (fun tipo -> (tipo, Infinitivo))
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           (vorto, ((if vorto.EndsWith("t") then TransitivaVerbo else DutransitivaVerbo), Infinitivo)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tre") -> Some (TransitivaVerbo, Progresivo)
           | v when v.EndsWith("pre") -> Some (DutransitivaVerbo, Progresivo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tro") -> Some (TransitivaVerbo, Perfekto)
           | v when v.EndsWith("pro") -> Some (DutransitivaVerbo, Perfekto)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tela") -> Some (TransitivaVerbo, Estonteco)
           | v when v.EndsWith("pela") -> Some (DutransitivaVerbo, Estonteco)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tora") -> Some (TransitivaVerbo, Ujo1Volo)
           | v when v.EndsWith("pora") -> Some (DutransitivaVerbo, Ujo1Volo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tore") -> Some (TransitivaVerbo, Ujo2Volo)
           | v when v.EndsWith("pore") -> Some (DutransitivaVerbo, Ujo2Volo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           if vorto.EndsWith("peri")
           then Some (DutransitivaVerbo, Ujo3Volo)
           else None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto DutransitivaVerbo) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("trie") -> Some (TransitivaVerbo, AtributivoEstiMalantaŭ)
           | v when v.EndsWith("prie") -> Some (DutransitivaVerbo, AtributivoEstiMalantaŭ)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tri") -> Some (TransitivaVerbo, Imperativo)
           | v when v.EndsWith("pri") -> Some (DutransitivaVerbo, Imperativo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("toniaa") -> Some (TransitivaVerbo, Argumento2)
           | v when v.EndsWith("poniaa") -> Some (DutransitivaVerbo, Argumento2)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 5)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tetio") -> Some (TransitivaVerbo, Argumento1)
           | v when v.EndsWith("petio") -> Some (DutransitivaVerbo, Argumento1)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 4)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("telit") -> Some (TransitivaVerbo, Translativo)
           | v when v.EndsWith("pelip") -> Some (DutransitivaVerbo, Translativo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 4)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tema") -> Some (TransitivaVerbo, Ĝerundo)
           | v when v.EndsWith("pema") -> Some (DutransitivaVerbo, Ĝerundo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tig") -> Some (TransitivaVerbo, PartaUjo1)
           | v when v.EndsWith("peg") -> Some (DutransitivaVerbo, PartaUjo1)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 2)
           (malinflektitaVorto, normaligi malinflektitaVorto (tipoDeInfinitivo malinflektitaVorto)) }

      { Kontroli = fun vorto ->
           match vorto with
           | v when v.EndsWith("tes") -> Some (TransitivaVerbo, PartaUjo2)
           | v when v.EndsWith("pesh") -> Some (DutransitivaVerbo, PartaUjo2)
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
           | v when v.EndsWith("tos") -> Some (TransitivaVerbo, Pasivigo)
           | v when v.EndsWith("posh") -> Some (DutransitivaVerbo, Pasivigo)
           | _ -> None
        Inflekti = neinflektebla
        Malinflekti = fun vorto ->
           let malinflektitaVorto = vorto.Substring(0, vorto.Length - 3)
           (malinflektitaVorto, normaligi malinflektitaVorto DutransitivaVerbo) }
      ]