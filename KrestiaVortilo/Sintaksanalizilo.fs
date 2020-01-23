namespace KrestiaVortilo

open Vorttipo
open System
open FSharpx.Collections

module Sintaksanalizilo =
   type SufiksoTabelo = SufiksoTabelo of FinajLiteroj: Map<string, Vorttipo * Inflekcio> * NefinajLiteroj: Map<string, SufiksoTabelo>

   /// Reprezentas la rezulton de unu malinflekto
   type MalinflektaŜtupo =
      // La vorto ne povis malinflektiĝi, ĉar ĝi jam estas en baza formo,
      // ekzemple la infinitivo, aŭ la vorttipo ne povas inflektiĝi.
      | Bazo of Vorttipo * Inflekcio
      // La vorto malinflektiĝis.
      | Nebazo of Vorttipo * Inflekcio * RestantaVorto: string

   let nombrigeblaInfinitivoFinaĵoj =
      [ "pu"; "po"; "paa"; "tu"; "to"; "taa"; "ku"; "ko"; "kaa" ]
   let nombrigeblaDifinitoFinaĵoj = [ "pi"; "pe"; "pa"; "ti"; "te"; "ta"; "ki"; "ke"; "ka" ]
   let nombrigeblaUnuNombroFinaĵoj = nombrigeblaDifinitoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "si")
   let nombrigeblaPluraNombroFinaĵoj = nombrigeblaDifinitoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ve")

   let nenombrigeblaInfinitivoFinaĵoj = [ "mu"; "mo"; "maa"; "nu"; "no"; "naa" ]
   let nenombrigeblaDifinitoFinaĵoj = [ "mi"; "me"; "ma"; "ni"; "ne"; "na" ]

   let finajLiteroj finaĵoj tipo inflekcio =
      finaĵoj
      |> List.map (fun finaĵo -> finaĵo, (tipo, inflekcio))
      |> Map.ofList

   let DUPFinaĵoj difinitoInflekcio unuInflekcio pluraInflekcio =
      finajLiteroj nombrigeblaDifinitoFinaĵoj NombrigeblaKlaso difinitoInflekcio
      |> Map.union (finajLiteroj nenombrigeblaDifinitoFinaĵoj NenombrigeblaKlaso difinitoInflekcio)
      |> Map.union (finajLiteroj nombrigeblaUnuNombroFinaĵoj NombrigeblaKlaso unuInflekcio)
      |> Map.union (finajLiteroj nombrigeblaPluraNombroFinaĵoj NombrigeblaKlaso pluraInflekcio)

   let inflekcioPostfiksarbo =
      SufiksoTabelo
         (DUPFinaĵoj Difinito UnuNombro PluraNombro,
          [ "a",
            SufiksoTabelo
               ([ "d", (Pridiranto, UnuNombro) ] |> Map.ofList,
                [ "w",
                  SufiksoTabelo
                     (finajLiteroj nombrigeblaInfinitivoFinaĵoj NombrigeblaKlaso PredikativoEsti, Map.empty)
                  "g",
                  SufiksoTabelo
                     (finajLiteroj nombrigeblaInfinitivoFinaĵoj NombrigeblaKlaso AtributativoEstiMalantaŭ, Map.empty)
                  "v",
                  SufiksoTabelo
                     (finajLiteroj nombrigeblaInfinitivoFinaĵoj NombrigeblaKlaso AtributativoEstiAntaŭ, Map.empty)
                  "s",
                  SufiksoTabelo
                     (Map.empty,
                      [ "n", SufiksoTabelo(DUPFinaĵoj Havaĵo UnuHavaĵo PluraHavaĵo, Map.empty) ] |> Map.ofList)
                  "r", SufiksoTabelo(DUPFinaĵoj Sola UnuSola PluraSola, Map.empty) ]
                |> Map.ofList)
            "s",
            SufiksoTabelo
               (Map.empty,
                [ "i",
                  SufiksoTabelo
                     (Map.empty,
                      [ "r", SufiksoTabelo((DUPFinaĵoj Havado UnuHavado PluraHavado, Map.empty)) ] |> Map.ofList) ]
                |> Map.ofList) ]
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

      let (SufiksoTabelo(finajLiteroj, nefinajLiteroj)) = arbo
      proviTroviFinon finajLiteroj
      |> Option.orElseWith (fun () ->
            nefinajLiteroj
            |> Map.tryPick (fun sufikso _ -> sufikso.Length |> Some)
            |> Option.bind (fun sufiksoLongeco ->
                  let sekvaSufikso =
                     literoj
                     |> List.take sufiksoLongeco
                     |> System.String.Concat

                  let restantajLiteroj = literoj |> List.skip sufiksoLongeco
                  nefinajLiteroj.TryFind sekvaSufikso
                  |> Option.bind (fun sekvaArbo -> troviFinaĵon restantajLiteroj sekvaArbo)))


   let malinflekti (ĉeno: string): Result<MalinflektaŜtupo, string> =
      match ĉeno with
      | _ when ĉuFremdaVorto ĉeno -> Bazo(FremdaVorto, SolaFormo) |> Ok
      | _ when ĉuLokokupilo ĉeno -> Bazo(Lokokupilo, SolaFormo) |> Ok
      | _ ->
         let literoj =
            ĉeno.ToCharArray()
            |> List.ofArray
            |> List.rev
         troviFinaĵon literoj inflekcioPostfiksarbo
         |> Option.orElseWith (fun () -> ĉuInfinitivo ĉeno |> Option.map (fun vorttipo -> Bazo(vorttipo, Infinitivo)))
         |> Option.map Ok
         |> Option.defaultValue (Error(sprintf "%s estas nevalida" ĉeno))
