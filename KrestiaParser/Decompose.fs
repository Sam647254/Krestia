module KrestiaParser.Decompose

open System
open KrestiaParser.Phonotactics
open KrestiaParser.Utils
open KrestiaParser.WordType

type Decomposition<'a> = State<string, 'a>

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

let private decomposeWith (WI (suffix, inflection, _)): Decomposition<Inflection> =
   withState {
      let! previousRemaining = getState
      let isSuffix = previousRemaining.EndsWith(suffix)
      let remaining = previousRemaining.Substring(0, previousRemaining.Length - suffix.Length)
      let isRemainingValid = isValidWord remaining
      return failwith "???"
   }

let private readSpecialWord word =
   result {
      match word with
      | _ when isTerminalDigit word -> return! Ok(BaseStep TerminalDigit, word)
      | _ when isNonterminalDigit word -> return (BaseStep NonterminalDigit, word)
      | _ ->
         let firstLetter = word.Chars 0

         if Char.IsUpper(firstLetter) then
            return (BaseStep Name, word)
         else
            return! Error(InvalidBaseWordError word)
   }

let private decomposeSpecialWord word =
   result {
      let! baseStep, baseWord = readSpecialWord word
      return ([ baseStep ], baseWord)
   }
   
let private decomposeWith (WI (suffix, inflection, wordTypes)) validTypes word =
   failwith "???"

let rec private decomposeWord validWordTypes word = failwith "???"

and private decomposeWordAcc validSuffixes validWordTypes stepsAcc word = failwith "???"

let private decomposeAll wordTypes word =
   decomposeSpecialWord word
   <|> decomposeWord suffixesList wordTypes List.empty word

let decompose (word: string) : Result<DecomposedWord, DecomposeError> =
   result {
      let! steps, baseWord = decomposeAll [ All ] word
      let steps' = List.rev steps

      let inflections =
         steps'
         |> List.tail
         |> List.map
               (fun step ->
                  match step with
                  | SecondaryStep (i, _) -> i
                  | _ -> failwith "Unreachable state")

      let baseStep =
         match List.head steps' with
         | BaseStep wordType -> wordType
         | _ -> failwith "Unreachable state"

      return
         { steps = inflections
           baseType = baseStep
           baseWord = baseWord }
   }
