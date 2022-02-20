module KrestiaParser.DictionaryHelper

open KrestiaParser.Decompose
open KrestiaParser.WordType

let typeNameOf word =
   let decomposedWord =
      decompose word
      |> Option.defaultWith (fun () -> failwithf $"Cannot decompose %s{word}")

   match decomposedWord.baseType with
   | CountableNoun -> "Countable noun"
   | UncountableNoun -> "Uncountable noun"
   | CountableAssociativeNoun -> "Countable associative noun"
   | UncountableAssociativeNoun -> "Uncountable associative noun"
   | Record -> "Record"
   | Verb1 -> "1-Verb"
   | Verb2 -> "2-Verb"
   | Verb3 -> "3-Verb"
   | Verb12 -> "1-2-Verb"
   | Verb13 -> "1-3-Verb"
   | Verb23 -> "2-3-Verb"
   | Verb123 -> "1-2-3-Verb"
   | Verb0 -> "0-Verb"
   | Pronoun -> "Pronoun"
   | Modifier -> "Modifier"
   | Name -> "Identifier"
   | TerminalDigit -> "Terminal digit"
   | NonterminalDigit -> "Non-terminal digit"

let stemOfWord word =
   let decomposedWord =
      decompose word
      |> Option.defaultWith (fun () -> failwithf $"Cannot decompose %s{word}")

   if isVerb decomposedWord.baseType then
      if decomposedWord.baseType <> Verb13 then
         word.Substring(0, word.Length - 2)
      else
         word.Substring(0, word.Length - 1)
   else
      word

let inflectedFormsOf word =
   let decomposedWord =
      decompose word
      |> Option.defaultWith (fun () -> failwithf $"Cannot decompose %s{word}")

   suffixesList
   |> List.choose
         (fun (WI (suffix, inflection, validTypes)) ->
            if List.contains decomposedWord.baseType validTypes then
               Some(word + suffix, inflection)
            else
               None)
