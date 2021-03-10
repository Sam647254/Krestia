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
      | Nebazo of Vorttipo * Inflekcio * RestantaVorto: string * Finaĵo: string

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

   let nombrigeblaPredikativoEstiFinaĵoj =
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
        "voniaa"
        // Aganto
        "tetio"
        "petio"
        "setio"
        "shetio" ]
      @ (verboFinaĵoj
         |> Map.values
         |> Seq.map (fun sufikso ->
               [ sufikso + "lo"
                 sufikso + "laa"
                 sufikso + "lu" ])
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
        "ka" ]
      @ (verboFinaĵoj
         |> Map.values
         |> Seq.map (fun sufikso ->
               [ sufikso + "le"
                 sufikso + "la"
                 sufikso + "li" ])
         |> Seq.concat
         |> Seq.toList)

   let nombrigeblaUnuNombroFinaĵoj =
      nombrigeblaDifinitoFinaĵoj
      |> List.map (fun finaĵo -> finaĵo + "si")

   let nombrigeblaPluraNombroFinaĵoj =
      nombrigeblaDifinitoFinaĵoj
      |> List.map (fun finaĵo -> finaĵo + "ve")

   let nenombrigeblaInfinitivoFinaĵoj =
      let klasajFinaĵoj = [ "mu"; "mo"; "maa"; "nu"; "no"; "naa" ]

      klasajFinaĵoj
      @ ((nombrigeblaDifinitoFinaĵoj @ klasajFinaĵoj
          |> List.map (fun finaĵo -> finaĵo + "ro")))

   let nebazajNenombrigeblaInfinitivoFinaĵoj =
      [ "mu"; "mo"; "maa"; "nu"; "no"; "naa" ]
      |> List.map (fun finaĵo -> finaĵo + "ro")

   let nenombrigeblaDifinitoFinaĵoj =
      [ "mi"; "me"; "ma"; "ni"; "ne"; "na" ]
      @ ((nombrigeblaDifinitoFinaĵoj
          @ [ "mu"; "mo"; "maa"; "nu"; "no"; "naa" ]
          |> List.map (fun finaĵo -> finaĵo + "re")))

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
        DutransitivaVerbo ]
      |> Set.ofList

   let malplenigeblaVerboTipoj =
      [ MalplenaVerbo, [ MalplenaVerbo ]
        NetransitivaVerbo, [ NetransitivaVerbo; MalplenaVerbo ]
        OblikaNetransitivaVerbo,
        [ OblikaNetransitivaVerbo
          MalplenaVerbo ]
        NedirektaNetransitivaVerbo,
        [ NedirektaNetransitivaVerbo
          MalplenaVerbo ]
        TransitivaVerbo,
        [ TransitivaVerbo
          NetransitivaVerbo
          OblikaNetransitivaVerbo
          MalplenaVerbo ]
        NedirektaTransitivaVerbo,
        [ NedirektaTransitivaVerbo
          NetransitivaVerbo
          NedirektaNetransitivaVerbo
          MalplenaVerbo ]
        OblikaTransitivaVerbo,
        [ OblikaTransitivaVerbo
          OblikaNetransitivaVerbo
          NedirektaNetransitivaVerbo
          MalplenaVerbo ]
        DutransitivaVerbo, verboTipoj |> List.ofSeq ]
      |> List.map (fun (originala, malplenigitaj) -> originala, malplenigitaj |> Set.ofList)
      |> Map.ofList

   let bazajFinaĵoj =
      (nombrigeblaDifinitoFinaĵoj
       |> List.map (fun finaĵo -> finaĵo, NombrigeblaKlaso))
      @ (nenombrigeblaDifinitoFinaĵoj
         |> List.map (fun finaĵo -> finaĵo, NenombrigeblaKlaso))
        @ [ "lu", MalantaŭRekordo
            "li", AntaŭRekordo
            "dri", MalantaŭNombrigeblaEco
            "dre", AntaŭNombrigeblaEco
            "gri", MalantaŭNenombrigeblaEco
            "gre", AntaŭNenombrigeblaEco
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

   let ĉuFremdaVorto (ĉeno: string) =
      ĉeno.Length > 0
      && not (Char.IsLower(ĉeno.Chars(0)))

   let ĉuLokokupilo (ĉeno: string) =
      ĉeno.StartsWith("w") || ĉeno.StartsWith("h")

   let nefinajCiferoj =
      [ "mi", "0"
        "po", "1"
        "vo", "2"
        "no", "3"
        "te", "4"
        "si", "5"
        "li", "6"
        "so", "7"
        "ke", "8"
        "gi", "9"
        "plo", "00"
        "tri", "000"
        "kle", "000000"
        "di", "." ]
      |> Map.ofList

   let komencajNefinajCiferoj =
      [ "plo", "100"
        "tri", "1000"
        "kle", "1000000"
        "di", "0." ]
      |> Map.ofList

   let finajCiferoj =
      [ "mira", "0"
        "pona", "1"
        "vora", "2"
        "nona", "3"
        "tera", "4"
        "sina", "5"
        "lira", "6"
        "sona", "7"
        "kera", "8"
        "gina", "9"
        "trira", "00"
        "plora", "000"
        "klera", "0000000" ]
      |> Map.ofList

   let komencajFinajCiferoj =
      [ "volira", "100"
        "nolira", "1000"
        "linara", "1000000" ]
      |> Map.ofList

   let unarajOperaccioj: Map<string, (double -> double)> =
      [ "mebel", Math.Sqrt
        "vobel", (fun a -> a * a)
        "evotel", Math.Log ]
      
      |> Map.ofList
   let binarajOperaciioj: Map<string, (double -> double -> double)> =
      [ "tikal", (+)
        "senal", (-)
        "petal", (*)
        "visal", (/)
        "vetel", (fun a b -> Math.Pow(a, b)) ]
      |> Map.ofList

   let ĉuNefinaCifero vorto = Map.containsKey vorto nefinajCiferoj

   let ĉuFinaCifero vorto = Map.containsKey vorto finajCiferoj

   let ĉuCifero vorto =
      ĉuNefinaCifero vorto || ĉuFinaCifero vorto

   let ĉuBazo (ĉeno: string): Vorttipo option =
      if ĉuFremdaVorto ĉeno then
         Some FremdaVorto
      elif ĉuLokokupilo ĉeno then
         Some Lokokupilo
      elif ĉuCifero ĉeno then
         Some Cifero
      else
         bazajFinaĵoj
         |> Map.tryPick (fun finaĵo tipo ->
               if ĉeno.EndsWith(finaĵo)
                  && ĉeno.Length > finaĵo.Length then
                  Some tipo
               else
                  None)

   let bazaInflekcioDe vorttipo =
      match vorttipo with
      | NombrigeblaKlaso
      | NenombrigeblaKlaso
      | AntaŭNenombrigeblaEco
      | AntaŭNombrigeblaEco
      | MalantaŭNenombrigeblaEco
      | MalantaŭNombrigeblaEco -> Difinito
      | MalplenaVerbo
      | NetransitivaVerbo
      | OblikaNetransitivaVerbo
      | NedirektaNetransitivaVerbo
      | TransitivaVerbo
      | DutransitivaVerbo
      | OblikaTransitivaVerbo
      | NedirektaTransitivaVerbo -> Progresivo
      | _ -> SolaFormo

   let ĉuReciproka (infinitivo: string) =
      [ "at"; "ap"; "av"; "ash" ]
      |> List.exists infinitivo.EndsWith

   let infinitivoNomoDe (ĉeno: string): string option =
      if ĉuFremdaVorto ĉeno then
         Some "Foreign word"
      elif ĉuLokokupilo ĉeno then
         Some "Placeholder"
      elif ĉuCifero ĉeno then
         Some "Digit"
      else
         ĉuBazo ĉeno
         |> Option.map (fun tipo ->
               match tipo with
               | NombrigeblaKlaso -> "Countable class"
               | NenombrigeblaKlaso -> "Uncountable class"
               | MalantaŭRekordo -> "Record (postfix)"
               | AntaŭRekordo -> "Record (prefix)"
               | MalantaŭNombrigeblaEco -> "Countable associative class (postfix)"
               | AntaŭNombrigeblaEco -> "Countable associative class (prefix)"
               | MalantaŭNenombrigeblaEco -> "Uncountable associative class (postfix)"
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
               | FremdaVorto -> "Foreign word"
               | Cifero -> "Digit")
         |> Option.map (fun bazo -> if ĉuReciproka bazo then bazo + " (reciprocal)" else bazo)
