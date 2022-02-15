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

let private isValidNextInflection wordType suffix inflection =
   let hasValidSuffix =
      suffixesList
      |> List.tryFind (fun (WI (expectedSuffix, nextInflection, validBaseTypes)) ->
         inflection = nextInflection && List.contains wordType validBaseTypes && suffix = expectedSuffix)
      |> Option.isSome
   let validPI = lazy (inflection = PredicativeIdentity && canUsePI wordType)
   let validPostfix = lazy (inflection = Postfixed && canBePostfixed wordType)
   hasValidSuffix || validPI.Value || validPostfix.Value

let private decomposeWith (WI (suffix, inflection, _)): Decomposition<Inflection * string> =
   withState {
      let! previousRemaining = getState
      let isSuffix = previousRemaining.EndsWith(suffix)
      let remaining = previousRemaining.Substring(0, max 0 (previousRemaining.Length - suffix.Length))
      let isRemainingValid = isValidWord remaining
      if isSuffix && isRemainingValid then
         do! putState remaining
         return (inflection, suffix)
   }

let private validInflections = List.map decomposeWith suffixesList
   
let rec private validate baseType suffix nextInflection restInflections =
   match restInflections with
   | [] -> isValidNextInflection baseType suffix nextInflection
   | (inflection, nextSuffix) :: rest ->
      option {
         let! wordType, isTerminal = behaviourOf baseType nextInflection
         do! takeIf (isValidNextInflection wordType suffix nextInflection && (not isTerminal || List.isEmpty rest))
         return validate wordType nextSuffix inflection rest
      }
      |> Option.defaultValue false

let rec private validateDerivation (inflections, baseWord) =
   option {
      let! baseType, _ = runBaseWord baseWord
      match inflections with
      | [] -> return failwith "Invalid state"
      | (inflection, suffix) :: rest ->
         do! takeIf (isValidNextInflection baseType suffix inflection && validate baseType suffix inflection rest)
         return { steps = List.map fst inflections; baseType = baseType; baseWord = baseWord }
   }
   
and private readBaseType (wordType: WordType) typeGuard =
   withState {
      let! word = getState
      if typeGuard word then
         do! putState ""
         return wordType
   }

and private readBaseWord =
   withState {
      let! word = getState
      if isValidWord word then
         let! baseType = lift (baseTypeOf word)
         do! putState ""
         return baseType
   }

and private readName: Decomposition<WordType> =
   readBaseType Name (fun word -> Char.IsUpper(word.Chars 1))

and private readTerminalDigit: Decomposition<WordType> =
   readBaseType TerminalDigit isTerminalDigit

and private readNonterminalDigit: Decomposition<WordType> =
   readBaseType NonterminalDigit isNonterminalDigit

and private readFixedWord: Decomposition<WordType> =
   withState {
      return! readName
      return! readTerminalDigit
      return! readNonterminalDigit
   }

and private readPostfixed =
   withState {
      let! word = getState
      do! guard (isPostfixed word)
      do! putState (prefixToPostfix word)
      return (Postfixed, "")
   }

and private readPI =
   withState {
      let! word = getState
      do! guard (isPI word)
      do! putState (Option.get <| predicativeToDefinite word)
      return (PredicativeIdentity, "")
   }

and private runFixedWord = runState readFixedWord

and private runBaseWord = runState readBaseWord

let private addPreviousSteps inflections (inflection, word) = (inflection :: inflections, word)

let private step =
   List.append validInflections [ readPostfixed; readPI ]

let private runStep word =
   step
   |> List.map (fun s -> runState s word)
   |> List.choose id

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