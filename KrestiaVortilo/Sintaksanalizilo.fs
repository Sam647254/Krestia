namespace KrestiaVortilo

open Vorttipo
open System

module Sintaksanalizilo =
   type Prefiksarbo =
   | Fino of Vorttipo * Inflekcio
   | Nefino of Map<string, Prefiksarbo>

   type MalinflektaŜtupo =
   | Bazo of Vorttipo * Inflekcio * BazaVorto : string
   | Nebazo of Inflekcio * RestantajLiteroj : char list

   let nombrigeblaInfinitivoFinaĵoj =
      [ "pu"; "po"; "paa"; "tu"; "to"; "taa"; "ku"; "ko"; "kaa" ]

   let nenombrigeblaInfinitivoFinaĵoj = [ "mu"; "mo"; "maa"; "nu"; "no"; "naa" ]

   let inflekcioPostfiksarbo =
      Nefino(
         [ "a",
            Nefino(
               [ "d", Fino(Pridiranto, UnuNombro) ]
               |> Map.ofList)
         ] |> Map.ofList)

   let infinitivoFinaĵoj =
      (nombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo, NombrigeblaKlaso)) @
      (nenombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo, NenombrigeblaKlaso)) @
      [ "lu", MalantaŭRekordo
        "li", AntaŭRekordo
        "dru", MalantaŭNombrigeblaEco
        "dro", AntaŭNenombrigeblaEco
        "gru", MalantaŭNenombrieblaEco
        "gro", AntaŭNenombrigeblaEco
        "m", MalplenaVerbo
        "s", NetransitivaVerbo
        "ig", OblikaNetransitivaVerbo
        "n", NedirektaNetransitivaVerbo
        "t", TransitivaVerbo
        "sh", NedirektaTransitivaVerbo
        "eg", OblikaTransitivaVerbo
        "p", DutransitivaVerbo
        "d", Pridiranto
        "l", MalantaŭModifanto
        "r", AntaŭModifanto ]
      |> Map.ofList

   let ĉuFremdaVorto (ĉeno: string) = ĉeno.Length > 0 && not(Char.IsLower(ĉeno.Chars(0)))

   let ĉuLokokupilo (ĉeno: string) = ĉeno.StartsWith("w") || ĉeno.StartsWith("h")

   let ĉuInfinitivo (ĉeno: string): Vorttipo option =
      infinitivoFinaĵoj
      |> Map.tryPick (fun finaĵo tipo ->
         if ĉeno.EndsWith(finaĵo)
         then Some tipo
         else None)

   let rec troviFinaĵon (literoj: char list) (arbo: Prefiksarbo) (lastajLiteroj: string) =
      match arbo with
      | Fino(_, inflekcio) ->
         Nebazo(inflekcio, literoj |>
            List.append (lastajLiteroj.ToCharArray() |> List.ofArray)) |> Some
      | Nefino(subArbo) ->
         let sufiksoLongeco = subArbo |> Map.pick (fun sufikso _ -> sufikso.Length |> Some)
         let sekvaSufikso = literoj |> List.take sufiksoLongeco |> System.String.Concat
         let restantajLiteroj = literoj |> List.skip sufiksoLongeco
         subArbo.TryFind sekvaSufikso
         |> Option.bind (fun sekvaArbo -> troviFinaĵon restantajLiteroj sekvaArbo sekvaSufikso)

   let malinflekti (ĉeno: string): Result<MalinflektaŜtupo, string> =
      match ĉeno with
      | _ when ĉuFremdaVorto ĉeno -> Bazo(FremdaVorto, SolaFormo, ĉeno) |> Ok
      | _ when ĉuLokokupilo ĉeno -> Bazo(Lokokupilo, SolaFormo, ĉeno) |> Ok
      | _ ->
         ĉuInfinitivo ĉeno
         |> Option.map (fun vorttipo -> Bazo(vorttipo, Infinitivo, ĉeno) |> Ok)
         |> Option.orElseWith
            (fun () ->
               let literoj = ĉeno.ToCharArray() |> List.ofArray |> List.rev
               troviFinaĵon literoj inflekcioPostfiksarbo
               |> Option.map Ok)
         |> Option.defaultValue (Error (sprintf "%s estas nevalida" ĉeno))