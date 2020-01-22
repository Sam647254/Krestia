namespace KrestiaVortilo

open Vorttipo
open System

module Sintaksanalizilo =
   type SufiksoTabelo =
      | Fina of Map<string, Vorttipo * Inflekcio>
      | Nefina of Map<string, Vorttipo * Inflekcio> * Map<string, SufiksoTabelo>

   /// Reprezentas la rezulton de unu malinflekto
   type MalinflektaŜtupo =
      // La vorto ne povis malinflektiĝi, ĉar ĝi jam estas en baza formo,
      // ekzemple la infinitivo, aŭ la vorttipo ne povas inflektiĝi.
      | Bazo of Vorttipo * Inflekcio
      // La vorto malinflektiĝis.
      | Nebazo of Vorttipo * Inflekcio * RestantaVorto: string

   let nombrigeblaInfinitivoFinaĵoj =
      [ "pu"; "po"; "paa"; "tu"; "to"; "taa"; "ku"; "ko"; "kaa" ]

   let nenombrigeblaInfinitivoFinaĵoj = [ "mu"; "mo"; "maa"; "nu"; "no"; "naa" ]

   let finajLiteroj finaĵoj tipo inflekcio =
      finaĵoj
      |> List.map (fun finaĵo -> finaĵo, (tipo, inflekcio))
      |> Map.ofList

   let inflekcioPostfiksarbo =
      Nefina
         (Map.empty,
          [ "a",
            Nefina
               ([ "d", (Pridiranto, UnuNombro) ] |> Map.ofList,
                [ "w", Fina(finajLiteroj nombrigeblaInfinitivoFinaĵoj NombrigeblaKlaso PredikativoEsti) ] |> Map.ofList) ]
          |> Map.ofList)

   let infinitivoFinaĵoj =
      (nombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo, NombrigeblaKlaso))
      @ (nenombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo, NenombrigeblaKlaso))
        @ [ "lu", MalantaŭRekordo
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

   let ĉuFremdaVorto (ĉeno: string) = ĉeno.Length > 0 && not (Char.IsLower(ĉeno.Chars(0)))

   let ĉuLokokupilo (ĉeno: string) = ĉeno.StartsWith("w") || ĉeno.StartsWith("h")

   let ĉuInfinitivo (ĉeno: string): Vorttipo option =
      infinitivoFinaĵoj
      |> Map.tryPick (fun finaĵo tipo ->
            if ĉeno.EndsWith(finaĵo) then Some tipo
            else None)

   /// Provas trovi inflekcion por la finaĵo de la vorto (literoj).
   /// Se ĝi sukcesas, ĝi returnas Nebazon.
   let rec troviFinaĵon (literoj: char list) (arbo: SufiksoTabelo) =
      let proviTroviFinon kontrolajLiteroj =
         let restantajLiteroj =
            literoj
            |> List.rev
            |> System.String.Concat
         kontrolajLiteroj
         |> Map.tryPick (fun finaĵo tipoKajInflekcio ->
               if restantajLiteroj.EndsWith(finaĵo) then (Some tipoKajInflekcio)
               else None)
         |> Option.map (fun (tipo, inflekcio) -> Nebazo(tipo, inflekcio, restantajLiteroj))

      match arbo with
      | Fina(kontrolajLiteroj) -> proviTroviFinon kontrolajLiteroj
      | Nefina(kontrolajLiteroj, subtabeloj) ->
         proviTroviFinon kontrolajLiteroj
         |> Option.orElseWith (fun () ->
               let sufiksoLongeco = subtabeloj |> Map.pick (fun sufikso _ -> sufikso.Length |> Some)

               let sekvaSufikso =
                  literoj
                  |> List.take sufiksoLongeco
                  |> System.String.Concat

               let restantajLiteroj = literoj |> List.skip sufiksoLongeco
               subtabeloj.TryFind sekvaSufikso
               |> Option.bind (fun sekvaArbo -> troviFinaĵon restantajLiteroj sekvaArbo))


   let malinflekti (ĉeno: string): Result<MalinflektaŜtupo, string> =
      match ĉeno with
      | _ when ĉuFremdaVorto ĉeno -> Bazo(FremdaVorto, SolaFormo) |> Ok
      | _ when ĉuLokokupilo ĉeno -> Bazo(Lokokupilo, SolaFormo) |> Ok
      | _ ->
         ĉuInfinitivo ĉeno
         |> Option.map (fun vorttipo -> Bazo(vorttipo, Infinitivo) |> Ok)
         |> Option.orElseWith (fun () ->
               let literoj =
                  ĉeno.ToCharArray()
                  |> List.ofArray
                  |> List.rev
               troviFinaĵon literoj inflekcioPostfiksarbo |> Option.map Ok)
         |> Option.defaultValue (Error(sprintf "%s estas nevalida" ĉeno))
