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
   | BaseStep of (WordType * string)
   | InflectionStep of (Inflection * string)

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

let private isValidNextInflection wordType suffix inflection =
   let hasValidSuffix =
      suffixesList
      |> List.tryFind
            (fun (WI (expectedSuffix, nextInflection, validBaseTypes)) ->
               inflection = nextInflection
               && List.contains wordType validBaseTypes
               && suffix = expectedSuffix)
      |> Option.isSome

   let validPI =
      lazy
         (inflection = PredicativeIdentity
          && canUsePI wordType)

   let validPostfix =
      lazy (inflection = Postfixed && canBePostfixed wordType)

   hasValidSuffix
   || validPI.Value
   || validPostfix.Value

let private decomposeWith (WI (suffix, inflection, _)) : Decomposition<DecomposeStep> =
   state {
      let! previousRemaining = getState
      let isSuffix = previousRemaining.EndsWith(suffix)

      let remaining =
         previousRemaining.Substring(0, max 0 (previousRemaining.Length - suffix.Length))

      let isRemainingValid = isValidWord remaining

      if isSuffix && isRemainingValid then
         do! putState remaining
         return InflectionStep(inflection, suffix)
   }

let private validInflections = List.map decomposeWith suffixesList

let rec private hasValidSteps baseType inflections =
   match inflections with
   | [] -> true
   | InflectionStep(inflection, nextSuffix) :: rest ->
      option {
         let! wordType, isTerminal = behaviourOf baseType inflection

         if isValidNextInflection baseType nextSuffix inflection
            && (not isTerminal || List.isEmpty rest) then
            return hasValidSteps wordType rest
      }
      |> Option.defaultValue false
   | _ -> false

let private getInflections inflectionSteps =
   inflectionSteps
   |> List.map (fun step ->
      match step with
      | InflectionStep (i, _) -> i
      | s -> failwithf $"Invalid state %O{s}")

let rec private validateDerivation (inflectionSteps, _) =
   option {
      match inflectionSteps with
      | [] -> return failwith "Invalid state"
      | BaseStep (baseType, baseWord) :: rest ->
         if hasValidSteps baseType rest then
            return
               { steps = getInflections rest
                 baseType = baseType
                 baseWord = baseWord }
      | _ -> return! None
   }

and private readBaseType (wordType: WordType) typeGuard =
   state {
      let! word = getState

      if typeGuard word then
         do! putState ""
         return wordType
   }

and private readBaseWord =
   state {
      let! word = getState

      if isValidWord word then
         let! baseType = lift (baseTypeOf word)
         do! putState ""
         return BaseStep (baseType, word)
   }

and private readName: Decomposition<WordType> =
   readBaseType Name (fun word -> Char.IsUpper(word.Chars 1))

and private readTerminalDigit: Decomposition<WordType> =
   readBaseType TerminalDigit isTerminalDigit

and private readNonterminalDigit: Decomposition<WordType> =
   readBaseType NonterminalDigit isNonterminalDigit

and private readFixedWord: Decomposition<WordType> =
   state {
      return! readName
      return! readTerminalDigit
      return! readNonterminalDigit
   }

and private readPostfixed =
   state {
      let! word = getState

      if isPostfixed word then
         do! putState (prefixToPostfix word)
         return InflectionStep (Postfixed, "")
   }

and private readPI =
   state {
      let! word = getState

      if isPI word then
         do! putState (Option.get <| predicativeToDefinite word)
         return InflectionStep (PredicativeIdentity, "")
   }

and private runFixedWord = runState readFixedWord

let private addPreviousSteps inflections (inflection, word) = (inflection :: inflections, word)

let private step =
   List.append validInflections [ readPostfixed; readPI; readBaseWord ]

let private runStep word =
   step
   |> List.map (fun s -> runState s word)
   |> List.choose id

let rec private runStep' (inflectionsAcc, remainingWord) =
   let nextStep = runStep remainingWord

   let newList =
      List.map (addPreviousSteps inflectionsAcc) nextStep

   let nextStep' = List.collect runStep' newList
   List.append nextStep' newList

let decomposeWord (word: string) : Option<DecomposedWord> =
   let fixedWord =
      runFixedWord word
      |> Option.map fst
      |> Option.map
            (fun t ->
               { steps = []
                 baseType = t
                 baseWord = word })

   let commonWord () =
      let results = 
         runStep' ([], word)
      
      results
      |> List.choose validateDerivation
      |> List.tryExactlyOne

   fixedWord |> Option.orElseWith commonWord

let valencyOf word =
   decomposeWord word
   |> Option.map (fun word ->
      match word.baseType with
      | Verb1
      | Verb2
      | Verb3 -> 1
      | Verb12
      | Verb13
      | Verb23 -> 2
      | Verb123 -> 3
      | _ -> 0)
   |> Option.defaultValue 0