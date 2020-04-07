namespace KrestiaVortilo

open Vorttipo
open System
open FSharpx.Collections

module Sintaksanalizilo =
   /// Reprezentas la rezulton de unu malinflekto
   type MalinflektaŜtupo =
      // La vorto ne povis malinflektiĝi, ĉar ĝi jam estas en baza formo,
      // ekzemple la infinitivo, aŭ la vorttipo ne povas inflektiĝi.
      | Bazo of Vorttipo * Inflekcio * BazaVorto: string
      // La vorto malinflektiĝis.
      | Nebazo of Vorttipo * Inflekcio * RestantaVorto: string
   
   let verboFinaĵoj =
      [ MalplenaVerbo, "m"
        NetransitivaVerbo, "s"
        TransitivaVerbo, "t"
        NedirektaTransitivaVerbo, "sh"
        DutransitivaVerbo, "p"
        OblikaNetransitivaVerbo, "g"
        OblikaTransitivaVerbo, "v"
        NedirektaNetransitivaVerbo, "n" ]
      |> Map.ofList

   let nombrigeblaInfinitivoFinaĵoj =
      [ "pu"
        "po"
        "paa"
        "tu"
        "to"
        "taa"
        "ku"
        "ko"
        "kaa"
        // Patiento
        "toniaa"
        "poniaa"
        // Aganto
        "tetio"
        "petio"
        "setio"
        "shetio"
        // Eco
        "dro"
        "dru" ]
      @ (verboFinaĵoj
         |> Map.values
         |> Seq.map (fun sufikso -> [ sufikso + "lo"; sufikso + "laa"; sufikso + "lu" ])
         |> Seq.concat
         |> Seq.toList)

   let nombrigeblaDifinitoFinaĵoj =
      [ "pi"
        "pe"
        "pa"
        "ti"
        "te"
        "ta"
        "ki"
        "ke"
        "ka"
        // Patiento
        "tonia"
        "ponia"
        // Aganto
        "tetie"
        "petie"
        "setie"
        "shetie"
        // Eco
        "dre"
        "dri" ]
      @ (verboFinaĵoj
         |> Map.values
         |> Seq.map (fun sufikso -> [ sufikso + "le"; sufikso + "la"; sufikso + "li" ])
         |> Seq.concat
         |> Seq.toList)

   let nombrigeblaUnuNombroFinaĵoj = nombrigeblaDifinitoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "si")
   let nombrigeblaPluraNombroFinaĵoj = nombrigeblaDifinitoFinaĵoj |> List.map (fun finaĵo -> finaĵo + "ve")

   let nenombrigeblaInfinitivoFinaĵoj = [ "mu"; "mo"; "maa"; "nu"; "no"; "naa"; "gro"; "gru" ]
   let nenombrigeblaDifinitoFinaĵoj = [ "mi"; "me"; "ma"; "ni"; "ne"; "na"; "gre"; "gri" ]

   let finajLiteroj finaĵoj tipo inflekcio =
      finaĵoj
      |> List.map (fun finaĵo -> finaĵo, (tipo, inflekcio))
      |> Map.ofList

   let DUPFinaĵoj difinitoInflekcio unuInflekcio pluraInflekcio =
      finajLiteroj nombrigeblaDifinitoFinaĵoj NombrigeblaKlaso difinitoInflekcio
      |> Map.union (finajLiteroj nenombrigeblaDifinitoFinaĵoj NenombrigeblaKlaso difinitoInflekcio)
      |> Map.union (finajLiteroj nombrigeblaUnuNombroFinaĵoj NombrigeblaKlaso unuInflekcio)
      |> Map.union (finajLiteroj nombrigeblaPluraNombroFinaĵoj NombrigeblaKlaso pluraInflekcio)

   let verboTipoj =
      [ MalplenaVerbo
        NetransitivaVerbo
        OblikaNetransitivaVerbo
        NedirektaNetransitivaVerbo
        TransitivaVerbo
        NedirektaTransitivaVerbo
        OblikaTransitivaVerbo
        DutransitivaVerbo ] |> Set.ofList

   let malplenigeblaVerboTipoj =
      [ MalplenaVerbo, [ MalplenaVerbo ]
        NetransitivaVerbo, [ NetransitivaVerbo; MalplenaVerbo ]
        OblikaNetransitivaVerbo, [ OblikaNetransitivaVerbo; MalplenaVerbo ]
        NedirektaNetransitivaVerbo, [ NedirektaNetransitivaVerbo; MalplenaVerbo ]
        TransitivaVerbo, [ TransitivaVerbo; NetransitivaVerbo; OblikaNetransitivaVerbo; MalplenaVerbo ]
        NedirektaTransitivaVerbo,
        [ NedirektaTransitivaVerbo; NetransitivaVerbo; NedirektaNetransitivaVerbo; MalplenaVerbo ]
        OblikaTransitivaVerbo, [ OblikaTransitivaVerbo; OblikaNetransitivaVerbo; NedirektaNetransitivaVerbo ]
        DutransitivaVerbo, verboTipoj |> List.ofSeq ]
      |> List.map (fun (originala, malplenigitaj) -> originala, malplenigitaj |> Set.ofList)
      |> Map.ofList

   let infinitivoFinaĵoj =
      (nombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo, NombrigeblaKlaso))
      @ (nenombrigeblaInfinitivoFinaĵoj |> List.map (fun finaĵo -> finaĵo, NenombrigeblaKlaso))
        @ [ "lu", MalantaŭRekordo
            "li", AntaŭRekordo
            "dru", MalantaŭNombrigeblaEco
            "dro", AntaŭNombrigeblaEco
            "gru", MalantaŭNenombrieblaEco
            "gro", AntaŭNenombrigeblaEco
            "m", MalplenaVerbo
            "s", NetransitivaVerbo
            "g", OblikaNetransitivaVerbo
            "n", NedirektaNetransitivaVerbo
            "t", TransitivaVerbo
            "sh", NedirektaTransitivaVerbo
            "ʃ", NedirektaNetransitivaVerbo
            "v", OblikaTransitivaVerbo
            "p", DutransitivaVerbo
            "d", Pridiranto
            "l", MalantaŭModifanto
            "r", AntaŭModifanto ]
      |> Map.ofList

   let ĉuFremdaVorto (ĉeno: string) = ĉeno.Length > 0 && not (Char.IsLower(ĉeno.Chars(0)))

   let ĉuLokokupilo (ĉeno: string) = ĉeno.StartsWith("w") || ĉeno.StartsWith("h")

   let ĉuInfinitivo (ĉeno: string): Vorttipo option =
      if ĉuFremdaVorto ĉeno then
         Some FremdaVorto
      elif ĉuLokokupilo ĉeno then
         Some Lokokupilo
      else
         infinitivoFinaĵoj
         |> Map.tryPick (fun finaĵo tipo ->
               if ĉeno.EndsWith(finaĵo) && ĉeno.Length > finaĵo.Length then Some tipo
               else None)

   let infinitivoNomoDe (ĉeno: string): string option =
      if ĉuFremdaVorto ĉeno then
         Some "Foreign word"
      elif ĉuLokokupilo ĉeno then
         Some "Placeholder"
      else
         ĉuInfinitivo ĉeno
         |> Option.map (fun tipo ->
               match tipo with
               | NombrigeblaKlaso -> "Countable class"
               | NenombrigeblaKlaso -> "Uncountable class"
               | MalantaŭRekordo -> "Record (postfix)"
               | AntaŭRekordo -> "Record (prefix)"
               | MalantaŭNombrigeblaEco -> "Countable associative class (postfix)"
               | AntaŭNombrigeblaEco -> "Countable associative class (prefix)"
               | MalantaŭNenombrieblaEco -> "Uncountable associative class (postfix)"
               | AntaŭNenombrigeblaEco -> "Uncountable associative class (prefix)"
               | TransitivaVerbo -> "Transitive verb"
               | DutransitivaVerbo -> "Ditransitive verb"
               | NetransitivaVerbo -> "Intransitive verb"
               | NedirektaTransitivaVerbo -> "Indirect transitive verb"
               | MalplenaVerbo -> "Impersonal verb"
               | OblikaTransitivaVerbo -> "Oblique transitive verb"
               | OblikaNetransitivaVerbo -> "Oblique intransitive verb"
               | NedirektaNetransitivaVerbo -> "Indirect intransitive verb"
               | Pridiranto -> "Descriptor"
               | Lokokupilo -> "Placeholder"
               | MalantaŭModifanto -> "Modifier (postfix)"
               | AntaŭModifanto -> "Modifier (prefix)"
               | Makro -> "Macro"
               | FremdaVorto -> "Foreign word")
