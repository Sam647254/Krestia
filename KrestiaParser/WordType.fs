module KrestiaParser.WordType

open System
open KrestiaParser
open Utils

type WordType =
   | CountableNoun
   | UncountableNoun
   | CountableAssociativeNoun
   | UncountableAssociativeNoun
   | Record
   | Verb1
   | Verb2
   | Verb3
   | Verb12
   | Verb13
   | Verb23
   | Verb123
   | Verb0
   | Pronoun
   | Modifier
   | Name
   | TerminalDigit
   | NonterminalDigit

type Inflection =
   | Definite
   | Possession
   | PredicativeIdentity
   | AttributiveIdentityPrefix
   | AttributiveIdentityPostfix
   | Gerund
   | SpecificGerund
   | Possessive
   | Possessive0
   | Existence
   | Translative
   | Translative0
   | Detached
   | Lone
   | Progressive
   | Perfect
   | Intention
   | Hypothetical
   | Desiderative
   | Imperative
   | Hortative
   | Optative
   | Argument1
   | Argument2
   | Argument3
   | Partial1
   | Partial2
   | Partial3
   | Shift2
   | Shift3
   | Commencement
   | Reflection
   | Reflection1
   | Reflection3
   | Reflection0
   | Postfixed
   | Quality
   | SingleForm
   | NameI
   | DigitI
   | Predicate

type WI = WI of (string * Inflection * WordType list)

let private wi suffix inflection wordTypes = WI(suffix, inflection, wordTypes)

let suffixesList =
   [ wi
      "nsa"
      Possession
      [ CountableNoun
        UncountableNoun
        CountableAssociativeNoun
        UncountableAssociativeNoun ]
     wi
        "va"
        AttributiveIdentityPrefix
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "ga"
        AttributiveIdentityPostfix
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "vra"
        SpecificGerund
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "re"
        Quality
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "ra"
        Lone
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "rem"
        Possessive0
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "res"
        Possessive
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "rim"
        Existence
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "lam"
        Translative0
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]
     wi
        "las"
        Translative
        [ CountableNoun
          UncountableNoun
          CountableAssociativeNoun
          UncountableAssociativeNoun ]

     wi "io" Perfect [ Verb0; Verb2; Verb3; Verb23 ]
     wi "ia" Hypothetical [ Verb0; Verb2; Verb3; Verb23 ]
     wi
        "ela"
        Intention
        [ Verb0
          Verb1
          Verb12
          Verb13
          Verb123
          Verb2
          Verb23
          Verb3 ]
     wi
        "ea"
        Gerund
        [ Verb0
          Verb12
          Verb123
          Verb2
          Verb3
          Verb23 ]
     wi "ro" Perfect [ Verb12; Verb123 ]
     wi "o" Perfect [ Verb1; Verb13 ]
     wi "e" Hypothetical [ Verb1; Verb13 ]
     wi "ora" Desiderative [ Verb1; Verb12; Verb13; Verb123 ]
     wi "ea" Imperative [ Verb1; Verb13 ]
     wi "ri" Imperative [ Verb12; Verb123 ]
     wi "ie" Optative [ Verb1; Verb13; Verb3 ]
     wi "ra" Optative [ Verb2 ]
     wi "ri" Optative [ Verb23 ]
     wi "a" Hortative [ Verb1; Verb13 ]
     wi "etie" Argument1 [ Verb1; Verb12; Verb13; Verb123 ]
     wi "onia" Argument2 [ Verb12; Verb2; Verb123; Verb23 ]
     wi "eri" Argument3 [ Verb123; Verb13; Verb3; Verb123 ]
     wi "mea" Gerund [ Verb1; Verb13 ]
     wi "elim" Commencement [ Verb0 ]
     wi "elit" Commencement [ Verb12 ]
     wi "elis" Commencement [ Verb1 ]
     wi "elish" Commencement [ Verb13 ]
     wi "elip" Commencement [ Verb123 ]
     wi "em" Partial1 [ Verb1 ]
     wi "ig" Partial1 [ Verb12 ]
     wi "ev" Partial1 [ Verb123 ]
     wi "es" Partial2 [ Verb12 ]
     wi "am" Partial2 [ Verb2 ]
     wi "on" Partial2 [ Verb23 ]
     wi "osh" Partial2 [ Verb123 ]
     wi "ut" Partial3 [ Verb123 ]
     wi "ig" Partial3 [ Verb23 ]
     wi "ris" Reflection [ Verb12 ]
     wi "ish" Reflection [ Verb123 ]
     wi "es" Reflection [ Verb13 ]
     wi "is" Reflection1 [ Verb123 ]
     wi "im" Reflection0 [ Verb123 ]
     wi "rim" Reflection0 [ Verb12 ]
     wi "ret" Shift2 [ Verb12 ]
     wi "rop" Shift2 [ Verb123 ]
     wi "rup" Shift3 [ Verb123 ]
     wi "rosh" Shift3 [ Verb13 ]
     wi "riv" Shift3 [ Verb23 ] ]

let countableNounSuffixes =
   [ "pa"
     "pe"
     "pi"
     "ta"
     "te"
     "ti"
     "ka"
     "ke"
     "ki" ]

let uncountableNounSuffixes = [ "ma"; "me"; "mi"; "na"; "ne"; "ni" ]

let baseTypeOf (word: string) =
   if word.StartsWith("h") then
      Some Pronoun
   else if word.EndsWith("m") then
      Some Verb0
   else if List.tryFind (fun (suffix: string) -> word.EndsWith(suffix)) countableNounSuffixes
           |> Option.isSome then
      Some CountableNoun
   else if List.tryFind (fun (suffix: string) -> word.EndsWith(suffix)) uncountableNounSuffixes
           |> Option.isSome then
      Some UncountableNoun
   else
      None

let isVerb word =
   word = Verb0
   || word = Verb1
   || word = Verb2
   || word = Verb3
   || word = Verb12
   || word = Verb13
   || word = Verb23
   || word = Verb123

let isNoun word =
   word = CountableNoun
   || word = UncountableNoun
   || word = CountableAssociativeNoun
   || word = UncountableAssociativeNoun

let isPostfixed (word: string) =
   word.EndsWith("r")
   || ([ "dri"; "gri"; "dru"; "gru" ]
       |> List.tryFind word.EndsWith
       |> Option.isSome)

let predicativeToDefinite (word: string) =
   if word.EndsWith("aa") then
      Some <| word.Substring(0, word.Length - 2) + "a"
   else if word.EndsWith("o") then
      Some <| word.Substring(0, word.Length - 1) + "e"
   else if word.EndsWith("u") then
      Some <| word.Substring(0, word.Length - 1) + "i"
   else
      None

let definiteToPredicative (word: string) =
   let suffix =
      if word.EndsWith("a") then "aa"
      else if word.EndsWith("e") then "o"
      else "u"

   word.Substring(0, word.Length - 1) + suffix

let prefixToPostfix (word: string) =
   let suffix =
      if word.EndsWith("u") then
         "o"
      else if word.EndsWith("i") then
         "e"
      else if word.EndsWith("r") then
         "l"
      else
         failwithf $"Invalid postfix word %s{word}"

   word.Substring(0, word.Length - 1) + suffix

let isPI = predicativeToDefinite >> Option.isSome

let canUsePI wordType =
   wordType = CountableNoun
   || wordType = UncountableNoun
   || wordType = CountableAssociativeNoun
   || wordType = UncountableAssociativeNoun

let canBePostfixed wordType =
   wordType = Modifier
   || wordType = CountableAssociativeNoun
   || wordType = UncountableAssociativeNoun

let behaviourOf wordType inflection =
   if canUsePI wordType then
      match inflection with
      | PredicativeIdentity -> Some(Verb1, true)
      | Possession -> Some(wordType, true)
      | AttributiveIdentityPostfix -> Some(Modifier, true)
      | AttributiveIdentityPrefix -> Some(Modifier, true)
      | SpecificGerund -> Some(UncountableNoun, true)
      | Quality -> Some(UncountableNoun, false)
      | Lone -> Some(Verb0, false)
      | Possessive -> Some(Verb1, false)
      | Possessive0 -> Some(Verb0, false)
      | Translative -> Some(Verb1, false)
      | Translative0 -> Some(Verb0, false)
      | _ -> None
   else if isVerb wordType then
      None
   else
      None

type PositionedWord =
   { line: int
     position: int
     word: string }

let toPositionedWords (input: string) =
   input.Split([| Environment.NewLine |], StringSplitOptions.None)
   |> Array.map (fun line -> line.Split())
   |> Array.mapi
         (fun lineNumber line ->
            line
            |> Array.fold
                  (fun (position, words) next ->
                     let word =
                        { line = lineNumber
                          position = position
                          word = next }

                     (position + next.Length + 1, word :: words))
                  (0, [])
            |> snd
            |> List.rev)
   |> List.concat
   |> List.filter (fun word -> word.word.Length > 0)

let reducedForms =
   [ Verb0, Set.empty
     Verb1, Set.singleton Verb0
     Verb2, Set.singleton Verb0
     Verb3, Set.singleton Verb0
     Verb12, Set.ofList [ Verb0; Verb1; Verb2 ]
     Verb13, Set.ofList [ Verb0; Verb1; Verb3 ]
     Verb23, Set.ofList [ Verb0; Verb2; Verb3 ]
     Verb123,
     Set.ofList [ Verb0
                  Verb1
                  Verb2
                  Verb3
                  Verb12
                  Verb23
                  Verb13 ] ]
   |> Map.ofList

let verbEndings =
   [ Verb0, "m"
     Verb1, "s"
     Verb2, "g"
     Verb3, "n"
     Verb12, "t"
     Verb13, "sh"
     Verb23, "v"
     Verb123, "p" ]
   |> Map.ofList