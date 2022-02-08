module KrestiaParser.Decompose

open System
open KrestiaParser.Phonotactics
open KrestiaParser.Utils
open KrestiaParser.WordType

type Decomposition<'a> = OptionState<string, 'a>

type DecomposeError =
   | InvalidInflectionError of string
   | InvalidBaseWordError of string
   | OtherError of string

type DecomposedWord =
   { steps: Inflection list
     baseType: WordType
     baseWord: string }

type DecomposeStep =
   | BaseStep of WordType
   | InflectionStep of Inflection

type DecomposeResult = DecomposeStep list * string

let private isTerminalDigit word =
   match word with
   | "mira" -> true
   | "pona" -> true
   | "vora" -> true
   | "nona" -> true
   | "tera" -> true
   | "sina" -> true
   | "lira" -> true
   | "sona" -> true
   | "kera" -> true
   | "gina" -> true
   | "trira" -> true
   | "plora" -> true
   | "klera" -> true
   | _ -> false

let private isNonterminalDigit word =
   match word with
   | "mi" -> true
   | "po" -> true
   | "vo" -> true
   | "no" -> true
   | "te" -> true
   | "si" -> true
   | "li" -> true
   | "so" -> true
   | "ke" -> true
   | "gi" -> true
   | "di" -> true
   | "tri" -> true
   | "plo" -> true
   | "kle" -> true
   | _ -> false

let isValidNextInflection wordType inflection = failwith "???"

let private decomposeWith (WI (suffix, inflection, _)): Decomposition<Inflection> =
   withState {
      let! previousRemaining = getState
      let isSuffix = previousRemaining.EndsWith(suffix)
      let remaining = previousRemaining.Substring(0, previousRemaining.Length - suffix.Length)
      let isRemainingValid = isValidWord remaining
      if isSuffix && isRemainingValid then
         do! putState remaining
         return inflection
   }

let validateDerivation (inflections, baseWord) =
   option {
      return failwith "???"
   }
   
let private readBaseType (wordType: WordType) typeGuard =
   withState {
      let! word = getState
      if typeGuard word then
         do! putState ""
         return wordType
   }

let private readBaseWord =
   withState {
      let! word = getState
      if isValidWord word then
         let! baseType = lift (baseTypeOf word)
         do! putState ""
         return baseType
   }

let private readName: Decomposition<WordType> =
   readBaseType Name (fun word -> Char.IsUpper(word.Chars 1))

let private readTerminalDigit: Decomposition<WordType> =
   readBaseType TerminalDigit isTerminalDigit

let private readNonterminalDigit: Decomposition<WordType> =
   readBaseType NonterminalDigit isNonterminalDigit

let private readFixedWord: Decomposition<WordType> =
   withState {
      return! readName
      return! readTerminalDigit
      return! readNonterminalDigit
   }

let private runFixedWord = runState readFixedWord

let private runBaseWord = runState readBaseWord

let private addPreviousSteps inflections (inflection, word) = (inflection :: inflections, word)

let private runStep word: List<Inflection * string> = failwith "???"

let rec private runStep' (inflectionsAcc, remainingWord) =
   let nextStep = runStep remainingWord
   let newList = List.map (addPreviousSteps inflectionsAcc) nextStep
   let nextStep' = List.collect runStep' newList
   nonEmpty nextStep' newList

let decompose (word: string): Option<DecomposedWord> =
   let fixedWord =
      runFixedWord word
      |> Option.map fst
      |> Option.map (fun t -> { steps= []; baseType = t; baseWord = word })
   let commonWord () =
      let inflectedWord =
         runStep' ([], word)
         |> List.map validateDerivation
         |> catOptions
         |> List.tryHead
      let baseWord () =
         runBaseWord word
         |> Option.map fst
         |> Option.map (fun t -> { steps=[]; baseType=t; baseWord=word })
      
      inflectedWord
      |> Option.orElseWith baseWord
      
   fixedWord
   |> Option.orElseWith commonWord