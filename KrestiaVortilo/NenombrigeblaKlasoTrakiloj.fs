namespace KrestiaVortilo

open Vorttipo
open NombrigeblaKlasoTrakiloj

module NenombrigeblaKlasoTrakiloj =
   let nenombrigeblaInfinitivoFinaĵoj = [ "mu"; "mo"; "maa"; "nu"; "no"; "naa" ]
   let nenombrigeblaNekonitaNombroFinaĵoj = [ "mi"; "me"; "ma"; "nu"; "no"; "na" ]
   let ĝerundoFinaĵoj =
      nenombrigeblaNekonitaNombroFinaĵoj |> List.map (fun finaĵo -> finaĵo + "va")

   let normaligi infinitivo =
      match infinitivo with
      | _ -> (NenombrigeblaKlaso, Infinitivo)

   let trakiloj: Vorttraktilo list = [
      { Formo = (NenombrigeblaKlaso, Infinitivo)
        Kontroli = fun vorto ->
           nenombrigeblaInfinitivoFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto -> (vorto, (NenombrigeblaKlaso, Infinitivo)) }

      { Formo = (NenombrigeblaKlaso, NekonitaNombro)
        Kontroli = fun vorto ->
           nenombrigeblaNekonitaNombroFinaĵoj
           |> List.exists (fun finaĵo -> vorto.EndsWith(finaĵo))
        Inflekti = fun formo vorto -> failwith "???"
        Malinflekti = fun vorto ->
           let malinflektitaVorto =
              match vorto with
              | _ -> infinitivigi vorto
           (malinflektitaVorto, normaligi malinflektitaVorto) }
        ]